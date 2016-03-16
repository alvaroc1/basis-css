package com.gravitydev.basis.css

import fastparse._
import java.nio.file.{Files, Path, Paths}

object Css {
  
  type PosParser[T <: Node] = Parser[NodeRef[T]]
  
  private def positioned [T <: Node](parser: Parser[T]): PosParser[T] = {
    (Index ~ parser).map {case (idx, p) => NodeRef(p, LocalPosition(idx))}
  }
  
  val comment: Parser[String] = P {
    val commentChunk = CharsWhile(!"/*".contains(_)).! | !"*/" ~ AnyChar.! 
    "/*" ~ commentChunk.rep.map(_.mkString("")) ~ "*/"
  }
  
  val space: Parser[Unit] = P( 
    CharsWhile(" \n\t".contains(_))
  )
  
  val ws: Parser[Unit] = P(
    space.? ~ comment.?.map(_ => ()) ~ space.?
  )
  
  val id: Parser[String] = P( 
    (CharPred(CharPredicates.isLetter) | "-").rep.! 
  )
  
  val property: Parser[String] = P(
    (CharIn('a' to 'z') | "-").rep(1).!
  )
  
  val value = P( 
    CharsWhile(x => ! (";}!" contains x)).!.map(_.trim) 
  )
  
  val selector = P( 
    positioned(
      CharsWhile(_ != '{').rep(1).!.map(_.trim).map {Selector.apply}
    )
  ) 
  
  val rule = P( 
    positioned((selector ~ ws ~ declarationsBlock).map((Rule.apply _).tupled))
  )
  
  
  val declaration = P( 
    positioned(
      (property ~ ws ~ ":" ~ ws ~ value ~ ws ~ "!important".?.!.map(_.nonEmpty)).map {(Declaration.apply _).tupled}
    )
  )
  
  val statementSep = P( ws ~ ";".? ~ ws )
  
  val declarationsBlock = P {
    val declarationSep = ws ~ ";" ~ ws
    
    "{" ~ ws ~ declaration.rep(sep=declarationSep).map(_.toList) ~ statementSep ~ "}"
  }
 
  val rulesBlock = P(
    "{" ~ ws ~ rule.rep(sep=ws) ~ ws ~ "}"
  )
  
  val url = P {
    "url(" ~ (quoted | CharsWhile(_ != ')').!) ~ ")" |
    quoted
  }
  
  val quoted = P {
    ("\"" ~ CharsWhile(_ != '"').! ~ "\"") |
    ("'" ~ CharsWhile(_ != '\'').! ~ "'")
  }
  
  val atStatement = P(
    positioned(
      "@" ~! 
      (
        ("import" ~ space ~! url ~ CharsWhile(_ != ';').!.? ~ ";").map {case (url,rest) => Import(url, rest.getOrElse("").trim)} |
        (id ~ ";").map(x => AtStatement(x, "", Nil)) |                                // no params
        (id ~ space ~ CharsWhile(!";{".contains(_)).! ~ ";").map {case (i,cond) => AtStatement(i,cond,Nil)} | // param
        (id ~ space ~ CharsWhile(_ != '{').! ~! rulesBlock).map {case (i,cond,b) => AtStatement(i, cond.trim, b.toList)} // param and block
      )
    )
  )
  
  val statement: PosParser[Node with Statement] = P( atStatement | rule.map(x => x: NodeRef[Node with Statement]) )
  
  val stylesheet: PosParser[Stylesheet] = P( 
    ws ~ 
    positioned(
      (statement.rep(sep=statementSep).map(_.toList) ~ statementSep ~ End).map(Stylesheet.apply _)
    )
  )
  
  def parse (s: String) = stylesheet.parse(s) match {
    case Result.Success(r,_) => Right(r)
    case Result.Failure(e) => Left(e)
  }
  
  def processStylesheet(
    s: NodeRef[Stylesheet], 
    includeResolver: String => NodeRef[Stylesheet],
    processPosition: Position => Position
  ): NodeRef[Stylesheet] = {
    NodeRef(
      Stylesheet(
        s.value.rules.flatMap {
          case NodeRef(Import(url, ""), _) => includeResolver(url).value.rules
          case NodeRef(node, pos) => 
            NodeRef(
              visitChildren(
                node,
                new Visitor {
                  def apply [X <: Node](x: NodeRef[X]) = NodeRef(x.value, processPosition(x.pos))
                }
              ), 
              processPosition(pos)//pos.setFile(file)
            ) :: Nil
        }
      ),
      s.pos
    )
  }
  
  trait Visitor {
    def apply [X <: Node](x: NodeRef[X]): NodeRef[X]
  }
  
  def visitChildren [T <: Node](node: T, fn: Visitor): T = (node match {
    case Stylesheet(rules) => Stylesheet(
      rules.map(r => 
        fn(r).asInstanceOf[NodeRef[Node with Statement]]
      )
    )
    case Rule(sel, decls) => Rule(fn(sel), decls.map(fn.apply))
  }).asInstanceOf[T]
}
