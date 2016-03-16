package com.gravitydev.basis.css

import com.gravitydev.extras.DirectoryWatcher
import java.nio.file.{Path, Paths, Files, PathMatcher}
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json.Json
import java.util.concurrent.{ConcurrentHashMap, CopyOnWriteArraySet}
import scala.collection.JavaConverters._

class Build (
  sourceDir: Path, 
  targetDir: Path, 
  includePat: PathMatcher, 
  excludePat: PathMatcher
) {
  val codeMap = new ConcurrentHashMap[Path,String]()
  
  private def findSources (dir: Path): Iterable[Path] = {
    for {
      x <- Files.newDirectoryStream(dir).asScala
      f <- x match {
        case _ if Files.isDirectory(x) => findSources(x)
        case _ if includePat.matches(x) && !excludePat.matches(x) => x :: Nil
        case _ => Nil
      }
    } yield f
  }
  
  def compile (f: Path) = {
    Css.parse(getCode(f)).fold(
      e => println(e),
      ast => {
        val processed = Css.processStylesheet(
          ast,
          includeResolver(f, _),
          processPosition(f)
        )
        
        val compiled = CssPrinter.print(processed)
        val targetFile = targetDir.resolve(sourceDir.relativize(f))
        
        val parent = targetFile.getParent
        
        val sourceMap = parent.resolve(targetFile.getFileName.toString + ".map")
        
        Files.createDirectories(parent)
        
        println(compiled.positions)
        
        val sourceMapContent = Json.prettyPrint(
          Json.obj(
            "version"     -> 3,
            "file"        -> targetFile.getFileName.toString,
            "sourceRoot"  -> "",
            "sources"     -> compiled.positions.keySet.map(_.asInstanceOf[FilePosition].file.toString).toList,
            "names"       -> Json.arr("src", "maps", "are", "fun"),
            "mappings"    -> "A,AAAB;;ABCDE;"
          )  
        )
        
        Files.write(targetFile, compiled.layout.getBytes("UTF-8"))
        Files.write(sourceMap, sourceMapContent.getBytes("UTF-8"))
      }
    )
  }
  
  def getCode (f: Path) = Option(codeMap.get(f)) getOrElse {
    val contents = new String(Files.readAllBytes(f), "UTF-8")
    codeMap.put(f, contents)
    contents
  }
  
  def includeResolver (file: Path, url: String): NodeRef[Stylesheet] = {
    val includedFile = file.getParent.resolve(Paths.get(url))
    
    Css.processStylesheet(
      Css.parse(new String(Files.readAllBytes(includedFile), "UTF-8")).fold(
        e => {println(e); sys.error(e.toString)},
        identity
      ),
      includeResolver(includedFile, _),
      processPosition(includedFile)
    )
  }
  
  def processPosition(file: Path)(pos: Position): FilePosition2 = pos match {
    case LocalPosition(idx) => {
      FilePosition2(file, 0, idx)
    }
    case a: FilePosition2 => a.copy(file = file)
  }
  
  def start (): Unit = {
    // load all
    for (f <- findSources(sourceDir)) {
      codeMap.put(f, new String(Files.readAllBytes(f), "UTF-8"))
      compile(f)
    }
    
    DirectoryWatcher.watch(
      sourceDir, 
      c => {
        if (!excludePat.matches(c.path)) Compiler.handleChange(sourceDir, targetDir, c)
      },
      {c => includePat.matches(c.path)}, 
      println
    )
  }
}

object Compiler {
  
  def includeResolver (file: Path, url: String): NodeRef[Stylesheet] = {
    val includedFile = file.getParent.resolve(Paths.get(url))
    
    Css.processStylesheet(
      Css.parse(new String(Files.readAllBytes(includedFile), "UTF-8")).fold(
        e => {println(e); sys.error(e.toString)},
        identity
      ),
      includeResolver(includedFile, _),
      processPosition(includedFile)
    )
  }
  
  def processPosition(file: Path)(pos: Position): FilePosition2 = pos match {
    case LocalPosition(idx) => {
      FilePosition2(file, 0, idx)
    }
    case a: FilePosition2 => a.copy(file = file)
  }
              
  def handleChange (source: Path, target: Path, c: DirectoryWatcher.Change): Unit = {
    println("Change: " + c)
    
    c.kind match {
      case DirectoryWatcher.EventKind.Delete => println("Deleted")
      case _ => {
        Css.parse(new String(Files.readAllBytes(c.path), "UTF-8")).fold(
          e => println(e),
          ast => {
            val processed = Css.processStylesheet(
              ast,
              includeResolver(c.path, _),
              processPosition(c.path)
            )
            
            val compiled = CssPrinter.print(processed)
            val targetFile = target.resolve(source.relativize(c.path))
            
            val parent = targetFile.getParent
            
            val sourceMap = parent.resolve(targetFile.getFileName.toString + ".map")
            
            Files.createDirectories(parent)
            
            println(compiled.positions)
            
            val sourceMapContent = Json.prettyPrint(
              Json.obj(
                "version"     -> 3,
                "file"        -> targetFile.getFileName.toString,
                "sourceRoot"  -> "",
                "sources"     -> compiled.positions.keySet.map(_.asInstanceOf[FilePosition].file.toString).toList,
                "names"       -> Json.arr("src", "maps", "are", "fun"),
                "mappings"    -> "A,AAAB;;ABCDE;"
              )  
            )
            
            Files.write(targetFile, compiled.layout.getBytes("UTF-8"))
            Files.write(sourceMap, sourceMapContent.getBytes("UTF-8"))
          }
        )
      }
    }
  }
}

object BasisCSS extends App {
  val source = Paths.get(args(0))
  val target = Paths.get(args(1))
  val fs = source.getFileSystem
  val includePat = fs.getPathMatcher("glob:"+args(2))
  val excludePat = fs.getPathMatcher("glob:"+args(3))
  
  val build = new Build(
    sourceDir = Paths.get(args(0)),
    targetDir = Paths.get(args(1)),
    includePat = fs.getPathMatcher("glob:"+args(2)),
    excludePat = fs.getPathMatcher("glob:"+args(3))
  )
  build.start()
  
  DirectoryWatcher.watch(
    source, 
    c => {
      if (!excludePat.matches(c.path)) Compiler.handleChange(source, target, c)
    },
    {c => includePat.matches(c.path)}, 
    println
  )
  
  println("Running")
  
  Iterator.continually(scala.io.StdIn.readLine).takeWhile(l => false).foreach(_ => ())
}
