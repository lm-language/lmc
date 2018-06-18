package lmc

import lmc.common.{Loc, ScopeEntry, Symbol, TypeEntry}
import lmc.diagnostics._
import lmc.types._
import lmc.utils.Debug

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable


final class TypeChecker(
  private val ctx: Context.TC,
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private type E = Diagnostic => Unit

  private val Primitive = ctx.Primitive

  private val _symbolTypes = mutable.HashMap.empty[Symbol, Type]
  private val _typeVars = mutable.HashMap.empty[Symbol, Type]
  private val _symbolKinds = mutable.HashMap.empty[Symbol, Kind]
  private val _checked = mutable.Set.empty[Int]
  private val _generics = mutable.HashMap.empty[Int, Type]

  def check(node: P.Node)(implicit error: E): Unit = {
    checkWorker(node)
    node.children.foreach(check)
  }

  def checkWorker(node: P.Node)(implicit error: E): Unit = {
    if (!(_checked contains node.meta.id)) {
     node match {
        case i: P.Ident =>
          if (isBindingIdent(i)) {
            checkBindingVarIdent(i)
          } else {
            checkVarIdent(i)
          }
        case m: P.Declaration.Module =>
          checkModuleDecl(m)
        case P.Declaration.Let(_, _, pattern, Some(rhs)) =>
          val typ = inferExpr(rhs)
          assignTypeToPattern(pattern, typ)
        case _ => ()
      }
      _checked += node.meta.id
    }
  }

  def assignTypeToPattern(pattern: P.Pattern, t: Type): Unit = {
    pattern match {
      case P.Pattern.Var(_, i) =>
        pattern.scope.resolveSymbol(i.name) match {
          case Some(symbol) =>
            setTypeOfSymbol(symbol, t)
        }
    }
  }

  def inferExpr(expr: P.Expression)(implicit error: E): Type = {
    expr match {
      case P.Expression.Literal(_, P.Expression.Literal.LInt(_)) =>
        Primitive.Int
      case P.Expression.Prop(meta, e, prop) =>
        println(s"inferprop $e $prop")
        val modTyp = inferExpr(e)
        modTyp match {
          case Module(types, values) =>
            values.map({
              case (k, v) => k.text -> v
            }).get(prop.name) match {
              case Some(t) =>
                t
            }
        }
      case P.Expression.Var(_, ident) =>
        ident.scope
          .resolveSymbol(ident.name) match {
          case Some(symbol) =>
            getTypeOfSymbol(symbol) match {
              case Some(t) => t
              case None =>
                ctx.getDeclOf(symbol) match {
                  case Some(d) =>
                    check(d)
                    getTypeOfSymbol(symbol).getOrElse(Uninferred)
                  case None => Uninferred
                }
            }
          case None =>
            Uninferred
        }

    }
  }

  def checkBindingVarIdent(ident: P.Ident): Unit = {
    println("checkBindingIdent")
  }

  def checkModuleDecl(m: P.Declaration.Module): Unit = {
    val values = m.moduleScope.symbols.map({
      case (name, ScopeEntry(symbol, _)) =>
        val typ = ctx.makeGenericType(name)
        setTypeOfSymbol(symbol, typ)
        symbol -> typ
    })
    m.scope.resolveSymbol(m.ident.name) match {
      case Some(sym) =>
        setTypeOfSymbol(sym, Module(Map.empty, values.toMap))
      case None =>
        ???
    }
  }


  def checkVarIdent(ident: P.Ident)(implicit error: E): Unit = {
    ident.scope.resolveSymbol(ident.name) match {
      case Some(symbol) =>
        _symbolTypes.get(symbol) match {
          case Some(t) =>
            ()
          case None =>
            ctx.getDeclOf(symbol) match {
              case Some(d) =>
                check(d)
              case None =>
                ???
            }
            setTypeOfSymbol(symbol, ctx.makeGenericType(ident.name))
        }
      case None =>
        error(Diagnostic(
          loc = ident.loc,
          severity = Severity.Error,
          variant = UnBoundVar(ident.name)
        ))
    }
  }

  @tailrec def isBindingIdent(node: P.Node)(implicit error: E): Boolean = {
    val parentOpt = node.meta.parentId.flatMap(ctx.getParsedNode)
    parentOpt match {
      case Some(p: P.Pattern) => true
      case Some(node) => isBindingIdent(node)
      case None => false
    }
  }

  def getTypeOfSymbol(symbol: Symbol): Option[Type] = {
    _symbolTypes.get(symbol)
  }


  def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit = {
    _symbolTypes.get(symbol) match {
      case Some(e: ExistentialInstance) =>
        _generics.update(e.id, typ)
        for ((k, v) <- _symbolTypes) {
          v match {
            case ExistentialInstance(id, name) if e.id == id =>
              _symbolTypes.update(k, typ)
            case _ => ()
          }
        }
      case Some(_) => ???
      case None =>
        _symbolTypes.update(symbol, typ)
    }
  }

  def setTypeVar(symbol: Symbol, typ: Type): Unit = {
    _typeVars .update(symbol, typ)
  }


  def getKindOfSymbol(symbol: Symbol): Option[Kind] = {
    _symbolKinds.get(symbol)
  }

  def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit = {
    _symbolKinds.update(symbol, kind)
  }
}
