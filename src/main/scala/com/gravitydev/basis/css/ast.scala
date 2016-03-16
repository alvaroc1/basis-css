package com.gravitydev.basis.css

import fastparse._
import java.nio.file.{Files, Path, Paths}


trait Position {
  def setFile (f: Path): Position
}

case class LocalPosition(idx: Int) extends Position {
  def setFile (f: Path) = FilePosition(f, idx)
}
case class FilePosition(file: Path, idx: Int) extends Position {
  def setFile (f: Path) = FilePosition(f, idx)
}
case class FilePosition2(file: Path, column: Int, offset: Int) extends Position {
  def setFile (f: Path) = FilePosition2(f, column, offset)
}

case class NodeRef[+T <: Node](value: T, pos: Position)

sealed trait Node
sealed trait Statement {self: Node => }
case class Stylesheet(rules: List[NodeRef[Node with Statement]]) extends Node
case class Rule(selector: NodeRef[Selector], declarations: List[NodeRef[Declaration]]) extends Node with Statement
case class Import(url: String, rest: String) extends Node with Statement
case class AtStatement(id: String, param: String, block: List[NodeRef[Rule]]) extends Node with Statement
case class Selector(sel: String) extends Node 
case class Declaration (name: String, value: String, important: Boolean) extends Node

