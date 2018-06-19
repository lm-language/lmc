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

  def symbolTypes = _symbolTypes
  def generics = _generics

  def addConstraint(constraint: Constraint): Unit = {
    _constraints.append(constraint)
  }

  def constraints: Iterable[Constraint] = _constraints

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    val typ = ctx.makeGenericType(sourceFile.loc.path.toString)
    val declarations = sourceFile.declarations.map(d => inferDeclaration(d, None))
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
      case P.Declaration.Let(meta, modifiers, p: P.Pattern.Annotated, Some(rhs)) =>
        val typedPattern = inferPattern(p, moduleType)
        val typedRhs = checkExpression(rhs, typedPattern.meta.typ)
        T.Declaration.Let(
          meta.typed(typedRhs.meta.typ),
          modifiers.map(_.typed),
          typedPattern,
          Some(typedRhs)
        )
    }
  }

  private def inferPattern(p: P.Pattern, moduleType: Option[Type]): T.Pattern = {
    p match {
      case P.Pattern.Annotated(meta, inner, annotation) =>
        val typedAnnotation = inferTypeAnnotation(annotation)
        val typedInner = checkPattern(inner, typedAnnotation.meta.typ, moduleType)
        T.Pattern.Annotated(
          meta.typed(typedAnnotation.meta.typ),
          typedInner,
          typedAnnotation
        )
    }
  }

  private def inferTypeAnnotation(annotation: P.TypeAnnotation): T.TypeAnnotation = {
    annotation match {
      case P.TypeAnnotation.Var(meta, ident) =>
        val typedIdent = inferTypeVarIdent(ident)
        T.TypeAnnotation.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
    }
  }

  private def checkTypeAnnotation(annotation: P.TypeAnnotation, t: Type): T.TypeAnnotation = {
    val inferred = inferTypeAnnotation(annotation)
    addConstraint(Unifies(annotation.loc, t, inferred.meta.typ))
    inferred
  }

  private def inferTypeVarIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(symbol)) =>
        val typ = resolveTypeVar(symbol)
        T.Ident(
          ident.meta.typed(typ),
          symbol
        )
      case None =>
        T.Ident(
          ident.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = UnBoundTypeVar(ident.name)
            )
          ),
          ctx.makeSymbol(ident.name)
        )
    }
  }

  private def resolveTypeVar(symbol: Symbol): Type = {
    _typeVars.get(symbol) match {
      case Some(Var(sym)) =>
        resolveTypeVar(sym)
      case Some(t) => t
      case None => Var(symbol)
    }
  }

  private def checkExpression(e: P.Expression, typ: Type): T.Expression = {
    e match {
      case _ =>
        val typed = inferExpression(e)
        addConstraint(Unifies(e.loc, typ, typed.meta.typ))
        typed
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
    case P.Pattern.Paren(meta, inner) =>
      val typedInner = checkPattern(inner, typ, moduleType)
      T.Pattern.Paren(
        meta.typed(typ),
        typedInner
      )
    case P.Pattern.Annotated(meta, inner, annotation) =>
      val typedAnnotation = checkTypeAnnotation(annotation, typ)
      val typedInner = checkPattern(
        inner,
        typedAnnotation.meta.typ,
        moduleType)
      T.Pattern.Annotated(
        meta.typed(typedAnnotation.meta.typ),
        typedInner,
        typedAnnotation
      )
  }

  private def checkBindingVarIdent(ident: P.Ident, t: Type, moduleType: Option[Type]): T.Ident = {
    moduleType match {
      case Some(modTyp) =>
        addConstraint(HasDeclaration(ident.loc, modTyp, ident.name, t))
      case None =>
        ()
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
