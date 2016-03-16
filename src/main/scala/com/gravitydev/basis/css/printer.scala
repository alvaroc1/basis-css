package com.gravitydev.basis.css

import org.kiama.output.PrettyPrinter

object CssPrinter extends PrettyPrinter {
  
  def showPos (n: NodeRef[_]) = n.pos match {
    case FilePosition(f, i) => "/*" <+> n.value.getClass.getName <+> f.toString <> ":" <> i.toString <+> "*/"
    case LocalPosition(i) => "/*" <+> ":" <> i.toString <+> "*/"
  }
  
  def showRef (n: NodeRef[_ <: Node]): Doc = n match {
    case NodeRef(s: Stylesheet, _) => show(s)
    case _ => showPos(n) <> line <> positioned(n.pos, show(n.value))
  }
  
  def show (n: Node): Doc = n match {
    case Stylesheet(rules) => vsep(rules.map(showRef))
    case Import(url, rest) => "@import" <+> squotes(url) <> (if (rest.nonEmpty) space <> rest else "") <> ";"
    case AtStatement(id, "", Nil) => "@" <> id <> ";"
    case AtStatement(id, param, Nil) => "@" <> id <+> param <> ";"
    case AtStatement(id, cond, rules) => "@" <> id <+> cond <+> braces( nest( line <> vsep(rules map showRef) ) <> line )
    case Rule(sel, dec) => showRef(sel) <+> braces( nest(line <> vsep(dec.map(showRef))) <> line)
    case Declaration(prop, v, imp) => prop <> ":" <+> v <> (if (imp) space <> "!important" else "") <> ";"
    case Selector(v) => v
  }
  
  def showCompactRef (n: NodeRef[_ <: Node]): Doc = showCompact(n.value)
  
  def showCompact (n: Node): Doc = n match {
    case Stylesheet(rules) => ssep(rules.map(showCompactRef), "")
    case at @ AtStatement(_,_,Nil) => show(at)
    case AtStatement(id,param,rules) => "@" <> id <+> param <> braces(hsep(rules map showCompactRef))
    case Rule(sel, dec) => showCompactRef(sel) <> braces(ssep(dec.map(showCompactRef), ";"))
    case Declaration(prop, v, imp) => prop <> ":" <> v <> (if (imp) "!important" else "")
    case x => show(x)
  }
  
  def print (s: NodeRef[Stylesheet]) = {
    //val doc: String = showRef(s)
    
    pretty(showRef(s))
  }
  def printCompact(s: Stylesheet) = pretty(showCompact(s))
}
