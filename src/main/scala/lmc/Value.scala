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
    case Value.Call(Value.Var(v), arg) =>
      s"$v($arg)"
    case Value.Call(Value.ParamRef(s), arg) =>
      s"$s($arg)"
    case Value.Call(f, arg) =>
      s"($f)($arg)"
    case Value.ExternFunc(_) => s"Func(..)"
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
        utils.joinIterable(map.map({ case (k, v) => s"$k: $v" }), "; ")}; }"
    case Value.Uninferred =>
      s"Uninferred"
    case Value.TypeOfModuleMember(lhs, rhs) => s"$lhs::$rhs"
    case Value.Prop(lhs, rhs) => s"$lhs.$rhs"
    case Value.If(p, t, f) => s"if ($p) $t else $f"
    case Value.TaggedUnion(sym, args) if args.isEmpty =>
      s"$sym"
    case Value.TaggedUnion(sym, args) =>
      s"$sym(${utils.joinIterable(args.toList)})"
    case Value.NamedArrow(name, from,  to) =>
      s"($name: $from) -> $to"
    case Value.Lambda(param, value) =>
      s"fn ($param) => $value"
    case Value.ParamRef(symbol) => symbol.toString
  }
}
object Value {
  type Type = Value

  case class Constructor(symbol: Symbol) extends Value

  /**
    * A reference to a variable that has a bound value
    * It is different from a ParamRef which is used for as of yet
    * unbound params. The difference is that ParamRef can be substituted
    * with an actual value on application
    * E.g. let x = 4;
    *      let y = x; // here x will be represented as Var(x)
    * @param symbol
    */
  case class Var(symbol: Symbol) extends Value

  /**
    * Reference to a function param that can be substituted
    * @example
    *          let x = fn(a: Type) => a;
    *          let b = x;
    *
    *          // here, x will be represented as Var(x) because
    *          // it already has a value (rhs of the first declaration)
    *          // inside the function body, `a` will be a ParamRef
    *          // because it doesn't have a value yet.
    *
    *          // if the function is applied to a value `y`, all instances
    *          // of TermRef(a) will be replaced with `y` in the body of
    *          // the function.
    * @param symbol
    */
  case class ParamRef(symbol: Symbol) extends Value

  case class Int(value: scala.Int) extends Value
  case class Bool(value: Boolean) extends Value
  case class ExternFunc(f: Value => Value) extends Value
  case class Call(func: Value, arg: Value) extends Value
  case object Uninferred extends Value
  case object Unit extends Value
  case class If(predicate: Value, trueBranch: Value, falseBranch: Value) extends Value
  case class Arrow(from: Type, to: Type) extends Value
  case class NamedArrow(name: Symbol, from: Type, to: Type) extends Value
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
  case class Lambda(param: Symbol, body: Value) extends Value

  def arrow(first: Type, types: Type*) = {
    val args = first +: types
    val last = args.last
    val f = args.take(types.length)
    f.foldRight(last)(Arrow.apply)
  }
}


