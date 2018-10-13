package lmc

import lmc.common.Symbol

sealed trait Value {
  override def toString: String = this match {
    case Value.Constructor(symbol) => symbol.text
    case Value.GenericInstance(id: Int) => s"GenericInstance($id)"
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
    case Value.Module(map) =>
      s"module { ${
        utils.joinIterable(
          map.map({ case (k, v) => s"$k = $v" }), "; ")}; }"
    case Value.ModuleType(map) =>
      s"module type { ${
        utils.joinIterable(map.map({ case (k, v) => s"$k: $v" }), "; ")}}"
    case Value.Uninferred =>
      s"Uninferred"
    case Value.TypeOfModuleMember(lhs, rhs) => s"$lhs::$rhs"
    case Value.Prop(lhs, rhs) => s"$lhs.$rhs"
    case Value.If(p, t, f) => s"if ($p) $t else $f"
    case Value.TaggedUnion(sym, args) if args.isEmpty =>
      s"$sym"
    case Value.TaggedUnion(sym, args) =>
      s"$sym(${utils.joinIterable(args.toList)})"
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
  case class TaggedUnion(tag: Symbol, values: Array[Value]) extends Value
  case class GenericInstance(id: scala.Int) extends Value
  case class Let(name: Symbol, rhs: Value, in: Value) extends Value
  case class Module(map: Map[Symbol, Value]) extends Value {

    val symbolMap: Map[String, Symbol] = map.keys.map(s => s.text -> s).toMap

  }
  case class ModuleType(map: Map[Symbol, Value]) extends Value {
    val symbolMap: Map[String, Symbol] = map.keys.map(s => s.text -> s).toMap
  }
  case class TypeOfModuleMember(lhs: Value, rhs: String) extends Value
  case class TypeOf(symbol: Symbol) extends Value
  case class Prop(lhs: Value, rhs: String) extends Value

  def arrow(first: Type, types: Type*) = {
    val args = first +: types
    val last = args.last
    val f = args.take(types.length)
    f.foldRight(last)(Arrow.apply)
  }
}


