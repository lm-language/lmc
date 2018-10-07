package lmc

import lmc.common.Symbol

sealed trait Value {
  override def toString: String = this match {
    case Value.Constructor(symbol) => symbol.text
    case Value.Arrow(from, to) =>
      if (from.isInstanceOf[Value.Arrow]) {
        s"($from) -> $to"
      } else {
        s"$from -> $to"
      }
    case Value.Call(f, arg) => s"$f($arg)"
    case Value.Func(_) => s"Func(..)"
    case Value.Bool(b) => b.toString
    case Value.Int(i) => i.toString
    case Value.Var(v) => v.text
    case Value.TypeOf(s) =>
      s"typeof $s"
    case Value.ModuleType(map) =>
      s"ModuleType($map)"
    case Value.Module(map) => s"module { $map }"
    case Value.Uninferred =>
      s"Uninferred"
    case Value.TypeOfMember(lhs, rhs) => s"$lhs::$rhs"
    case Value.Prop(lhs, rhs) => s"$lhs.$rhs"
    case Value.If(p, t, f) => s"if ($p) $t else $f"
  }
}
object Value {
  type Type = Value

  case class Constructor(symbol: Symbol) extends Value
  case class Var(symbol: Symbol) extends Value
  case class Int(value: scala.Int) extends Value
  case class Bool(value: Boolean) extends Value
  case class Func(f: Value => Value) extends Value
  case class Call(func: Value, arg: Value) extends Value
  case object Uninferred extends Value
  case object Unit extends Value
  case class If(predicate: Value, trueBranch: Value, falseBranch: Value) extends Value
  case class Arrow(from: Type, to: Type) extends Value
  case class Module(map: Map[Symbol, Value]) extends Value
  case class ModuleType(map: Map[Symbol, Value]) extends Value
  case class TypeOfMember(lhs: Value, rhs: String) extends Value
  case class TypeOf(symbol: Symbol) extends Value
  case class Prop(lhs: Value, rhs: String) extends Value

  def arrow(first: Type, types: Type*) = {
    val args = first +: types
    val last = args.last
    val f = args.take(types.length)
    f.foldRight(last)(Arrow.apply)
  }
}


