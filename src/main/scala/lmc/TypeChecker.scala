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

  private val _constraints = mutable.ListBuffer.empty[Constraint]

  def addConstraint(constraint: Constraint): Unit = {
    _constraints.append(constraint)
  }

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    val typ = ctx.makeGenericType(sourceFile.loc.path.toString)
    val declarations = sourceFile.declarations.map(d => inferDeclaration(d, None))
    solveConstraints()
    for ((k, t) <- _symbolTypes) {
      _symbolTypes.update(k, applyEnv(t))
    }
    T.SourceFile(
      meta = sourceFile.meta.typed(typ),
      scope = sourceFile.scope,
      declarations = declarations
    )
  }

  private def inferDeclaration(decl: P.Declaration, moduleType: Option[Type]): T.Declaration = {
    decl match {
      case mod: P.Declaration.Module => inferModuleDeclaration(mod)
      case let: P.Declaration.Let => inferLetDeclaration(let, moduleType)
    }
  }

  private def inferLetDeclaration(let: P.Declaration.Let, moduleType: Option[Type]): T.Declaration.Let = {
    let match {
      case P.Declaration.Let(meta, modifiers, p: P.Pattern.Var, Some(rhs)) =>
        val typedRhs = inferExpression(rhs)
        val typedPattern = checkPattern(p, typedRhs.meta.typ, moduleType)
        T.Declaration.Let(
          meta.typed(Primitive.Unit),
          modifiers.map(_.typed),
          typedPattern,
          Some(typedRhs)
        )
    }
  }

  private def inferExpression(e: P.Expression): T.Expression = e match {
    case P.Expression.Literal(meta, P.Expression.Literal.LInt(x)) =>
      T.Expression.Literal(meta.typed(Primitive.Int), T.Expression.Literal.LInt(x))
    case P.Expression.Prop(meta, expr, prop) =>
      val typedExpr = inferExpression(expr)
      val typedProp = inferBindingVarIdent(prop)
      addConstraint(HasProperty(typedProp.loc, typedExpr.meta.typ, prop.name, typedProp.meta.typ))
      T.Expression.Prop(
        meta.typed(typedProp.meta.typ),
        typedExpr,
        typedProp
      )
    case P.Expression.Var(meta, ident) =>
      val typedIdent = inferVarIdent(ident)
      T.Expression.Var(
        meta.typed(typedIdent.meta.typ),
        typedIdent
      )
  }

  private def inferVarIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolveSymbol(ident.name) match {
      case Some(symbol) =>
        val typ = getTypeOfSymbol(symbol) match {
          case Some(t) =>
            t
          case None =>
            ctx.makeGenericType(symbol.text)
        }
        setTypeOfSymbol(symbol, typ)
        T.Ident(
          ident.meta.typed(typ),
          symbol
        )
      case None => ???
    }
  }

  private def checkPattern(p: P.Pattern, typ: Type, moduleType: Option[Type]): T.Pattern = p match {
    case P.Pattern.Var(meta, ident) =>
      T.Pattern.Var(
        meta.typed(typ),
        checkBindingVarIdent(ident, typ, moduleType)
      )
  }

  private def checkBindingVarIdent(ident: P.Ident, t: Type, moduleType: Option[Type]): T.Ident = {
    moduleType match {
      case Some(modTyp) =>
        addConstraint(HasDeclaration(ident.loc, modTyp, ident.name, t))
    }
    ident.scope.symbols.get(ident.name) match {
      case Some(ScopeEntry(symbol, _)) =>
        setTypeOfSymbol(symbol, t)
        T.Ident(
          ident.meta.typed(t),
          symbol
        )
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        setTypeOfSymbol(symbol, t)
        T.Ident(
          ident.meta.typed(t),
          symbol
        )
    }
  }

  private def inferModuleDeclaration(module: P.Declaration.Module): T.Declaration = {
    val ident = inferBindingVarIdent(module.ident)
    val declarations = module.body.map(d => inferDeclaration(d, Some(ident.meta.typ)))
    val modTyp = Module(Map.empty, module.moduleScope.symbols.map({
      case (k, t) => t.symbol -> getTypeOfSymbol(t.symbol).getOrElse(ctx.makeGenericType(t.symbol.text))
    }).toMap)
    addConstraint(Unifies(ident.loc, ident.meta.typ, modTyp))
//    val genericIdents = inferGenericIdents()
    T.Declaration.Module(
      meta = module.meta.typed(ident.meta.typ),
      module.modifiers.map(_.typed),
      ident,
      module.scope,
      Array(),
      body = declarations
    )
  }


  private def inferBindingVarIdent(ident: P.Ident): T.Ident = {
    val symbol = ident.scope.getSymbol(ident.name).get
    val typ = getTypeOfSymbol(symbol) match {
      case Some(t) => t
      case None => ctx.makeGenericType(ident.name)
    }
    setTypeOfSymbol(symbol, typ)
    T.Ident(
      meta = ident.meta.typed(typ),
      name = symbol
    )
  }


  def solveConstraints(constraints: List[Constraint] = this._constraints.toList): Unit = {
    constraints match {
      case Nil => Nil
      case hd::tl =>
        hd match {
          case h: HasDeclaration =>
            solveConstraints(hasDeclaration(h, tl))
          case u: Unifies =>
            solveConstraints(unifies(u, tl))
          case h: HasProperty =>
            solveConstraints(hasProperty(h, tl))
        }
    }
  }

  private def unifies(u: Unifies, constraints: List[Constraint]): List[Constraint] = {
    (u.expected, u.found) match {
      case (ExistentialInstance(id, _), t) =>
        _generics.update(id, t)
        constraints.map(applyEnv)
      case (t, ExistentialInstance(id, _)) =>
        _generics.update(id, t)
        constraints.map(applyEnv)
      case _ =>
        constraints
    }
  }

  private def applyEnv(constraint: Constraint): Constraint = {
    constraint match {
      case HasProperty(loc, t1, prop, t2) =>
        HasProperty(loc, applyEnv(t1), prop, applyEnv(t2))
      case HasDeclaration(loc, t1, prop, t2) =>
        HasDeclaration(loc, applyEnv(t1), prop, applyEnv(t2))
      case Unifies(loc, t1, t2) =>
        Unifies(loc, applyEnv(t1), applyEnv(t2))
    }
  }

  def applyEnv(t: Type): Type = t match {
    case e@ExistentialInstance(i, n) =>
      _generics.get(i) match {
        case Some(t1) => t1
        case None => e
      }
    case Module(types, values) =>
      Module(types, values.mapValues(applyEnv))
    case _ => t
  }

  private def hasDeclaration(hasDecl: HasDeclaration, constraints: List[Constraint]): List[Constraint] = {
    constraints.map({
      case hasProp: HasProperty if hasProp.t == hasDecl.t && hasProp.prop == hasDecl.prop =>
        Unifies(hasProp.loc, hasProp.propType, hasDecl.propType)
      case c => c
    })
  }

  private def hasProperty(hasProp: HasProperty, constraints: List[Constraint]): List[Constraint] = {
    constraints.flatMap({
      case hasDecl: HasDeclaration if hasProp.t == hasDecl.t && hasProp.prop == hasDecl.prop =>
        List(Unifies(hasProp.loc, hasDecl.propType, hasProp.propType), hasDecl)
      case c => List(c)
    })
  }


  @tailrec def isBindingIdent(ident: P.Ident, _current: Option[P.Node] = None): Boolean = {
    val current = _current.getOrElse(ident)
    val parentOpt = current.meta.parentId.flatMap(ctx.getParsedNode)
    parentOpt match {
      case Some(p: P.Pattern) => true
      case Some(mod: P.Declaration.Module) if ident.meta.id == mod.ident.meta.id =>
        true
      case Some(node) => isBindingIdent(ident, Some(node))
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
          def go(t: Type): Unit = {
            t match {
              case ExistentialInstance(id, name) if e.id == id =>
                _symbolTypes.update(k, typ)
              case Module(_, values) =>
                values.values.foreach(go)
              case _ => ()
            }
          }
          go(v)
        }
      case Some(_) =>
        ()
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

sealed trait Constraint {
  def loc: Loc

  override def toString: String = this match {
    case Unifies(_, expected, found) => s"$expected == $found"
    case HasDeclaration(_, t, prop, propType) => s"let $t.$prop: $propType"
    case HasProperty(_, t, prop, propType) => s"$t.$prop == $propType"
  }
}
case class Unifies(loc: Loc, expected: Type, found: Type) extends Constraint
case class HasProperty(loc: Loc, t: Type, prop: String, propType: Type) extends Constraint
case class HasDeclaration(loc: Loc, t: Type, prop: String, propType: Type) extends Constraint
