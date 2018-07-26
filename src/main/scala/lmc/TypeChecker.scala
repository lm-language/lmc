package lmc

import lmc.common.{Loc, Scope, Symbol}
import lmc.diagnostics._
import lmc.types._
import lmc.utils.Debug

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
  private val _members = mutable.HashMap.empty[Int, mutable.HashMap[String, Type]]

  def symbolTypes = _symbolTypes
  def generics = _generics

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    assignHoles(sourceFile)
    val symbol = ctx.makeSymbol("SourceFile")
    val declarations = sourceFile.declarations.map(inferDeclaration)
    val typ = getModuleType(sourceFile.scope)
    val inferredSourceFile = T.SourceFile(
      meta = sourceFile.meta.typed(typ),
      scope = sourceFile.scope,
      declarations = declarations
    )
    val errors = ListBuffer.empty[Diagnostic]
    getErrors(errors, inferredSourceFile)
    inferredSourceFile.copy(
      meta = inferredSourceFile.meta.withDiagnostics(errors)
    )
  }

  private def inferDeclaration(declaration: P.Declaration): T.Declaration = {
    declaration match  {
      case l: P.Declaration.Let => inferLetDeclaration(l)
      case m: P.Declaration.Module => inferModuleDeclaration(m)
      case d: P.Declaration.TypeAlias => inferTypeAlias(d)
    }
  }

  private def inferModuleDeclaration(m: P.Declaration.Module): T.Declaration = {
    val inferredDeclarations = m.body.map(inferDeclaration)
    val t = getModuleType(m.moduleScope)
    T.Declaration.Module(
      m.meta.typed(t),
      m.modifiers.map(_.typed),
      checkBindingVarIdent(t)(m.ident),
      m.moduleScope,
      inferGenericParams(m.genericParams),
      inferredDeclarations
    )
  }

  private def checkBindingVarIdent(typ: Type)(ident: P.Ident): T.Ident = {
    ident.scope.typeMap.get(ident.name) match {
      case Some((forwardType, symbol)) =>
        val errors = ListBuffer.empty[Diagnostic]
        assertAssignability(errors, ident.loc)(typ, forwardType)
        val t = applyEnv(forwardType, Some(errors))
        setTypeOfSymbol(symbol, t)
        T.Ident(ident.meta.typed(t), symbol)
      case None =>
        // compiler bug
        ???
    }
  }

  private def inferGenericParams(params: Array[P.GenericParam]): Array[T.GenericParam] = {
    params.map(inferGenericParam)
  }

  private def inferGenericParam(param: P.GenericParam): T.GenericParam = {
    ???
  }



  private def inferTypeAnnotation(annotation: P.TypeAnnotation): T.TypeAnnotation = {
    annotation match {
      case P.TypeAnnotation.Var(meta, ident) =>
        val typedIdent = inferIdent(ident)
        T.TypeAnnotation.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
    }
  }

  private def inferTypeAlias(alias: P.Declaration.TypeAlias): T.Declaration.TypeAlias = {
    alias match {
      case P.Declaration.TypeAlias(meta, modifiers, ident, None, Some(rhs)) =>
        val typedRhs = inferTypeAnnotation(rhs)
        val typedIdent = checkBindingVarIdent(typedRhs.meta.typ)(ident)
        val typedModifiers = modifiers.map(_.typed)
        T.Declaration.TypeAlias(
          meta.typed(typedIdent.meta.typ),
          typedModifiers,
          typedIdent,
          None,
          Some(typedRhs)
        )
    }
  }

  private def inferLetDeclaration(let: P.Declaration.Let): T.Declaration = {
    let match {
      case P.Declaration.Let(meta, modifiers, pattern: P.Pattern.Var, Some(rhs)) =>
        val typedRhs = inferExpression(rhs)
        val typedPattern = checkPattern(typedRhs.meta.typ)(pattern)
        val typedModifiers = modifiers.map(_.typed)
        T.Declaration.Let(
          meta.typed(typedPattern.meta.typ),
          typedModifiers,
          typedPattern,
          Some(typedRhs)
        )
    }
  }

  private def checkPattern(typ: Type)(pattern: P.Pattern): T.Pattern = {
    pattern match {
      case P.Pattern.Var(meta, ident) =>
        val typedIdent = checkBindingVarIdent(typ)(ident)
        T.Pattern.Var(meta.typed(typedIdent.meta.typ), typedIdent)
    }
  }

  private def assertAssignability(errors: ListBuffer[diagnostics.Diagnostic], loc: Loc)
    (found: Type, expected: Type) = {
    (found, expected) match {
      case
        (Primitive.Int, Primitive.Int)
        | (Primitive.Bool, Primitive.Bool)
      => ()
      case (_, ExistentialInstance(i, _)) =>
        if (_generics.contains(i)) {
          throw new Error(s"Compiler bug: Tried to instantiate type hole twice")
        }
        _generics.update(i, found)
      case _ =>
        errors.append(
          Diagnostic(
            severity = Severity.Error,
            loc = loc,
            variant = NotAssignableTo(found, expected)
          )
        )
    }
  }

  private def inferExpression(expr: P.Expression): T.Expression = {
    import P.{Expression => E}
    expr match {
      case E.Literal(meta, E.Literal.LInt(x)) =>
        T.Expression.Literal(meta.typed(Primitive.Int), T.Expression.Literal.LInt(x))
      case E.Prop(meta, lhs, prop) =>
        val inferredLhs = inferExpression(lhs)
        applyEnv(inferredLhs.meta.typ) match {
          case m: Module =>
            m.symbolOfString.get(prop.name) match {
              case Some(symbol) =>
                m.values.get(symbol) match {
                  case Some(t) =>
                    val typedProp = T.Ident(prop.meta.typed(t), symbol)
                    T.Expression.Prop(
                      meta.typed(t),
                      inferredLhs,
                      typedProp
                    )
                  case None =>
                    // compiler bug
                    ???
                }
              case None =>
                // no such symbol
                ???
            }
          case _ =>
            // not a module
            ???
        }
      case E.Var(meta, ident) =>
        val typedIdent = inferIdent(ident)

        T.Expression.Var(
          meta.typed(typedIdent.meta.typ),
          typedIdent
        )
    }
  }

  private def inferIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolve(ident.name) match {
      case Some((typ, symbol)) =>
        T.Ident(ident.meta.typed(typ), symbol)
      case None =>
        T.Ident(
          ident.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              severity = Severity.Error,
              loc = ident.loc,
              variant = UnBoundVar(ident.name)
            )
          ),
          ctx.makeSymbol(ident.name)
        )
    }
  }

  private def getModuleType(scope: Scope): Type = {
    Module(
      scope.typeMap.map({
        case (_, (t, sym)) =>
            sym -> t
      }).toMap
    )
  }

  private def assignHoles(node: P.Node): Unit = {
    def getSymbol(ident: P.Ident): Symbol = {
//      ctx.makeSymbol(ident.name)
      ident.scope.getSymbol(ident.name).get
    }
    node match {
      case p: P.Pattern.Var =>
        val symbol = getSymbol(p.ident)
        val typ = ctx.makeGenericType(p.ident.name)
        p.scope.setType(p.ident.name, symbol, typ)
      case m: P.Declaration.Module =>
        val symbol = getSymbol(m.ident)
        val typ = ctx.makeGenericType(m.ident.name)
        m.scope.setType(m.ident.name, symbol, typ)
      case m: P.Declaration.TypeAlias =>
        val symbol = getSymbol(m.ident)
        val typ = ctx.makeGenericType(m.ident.name)
        m.scope.setType(m.ident.name, symbol, typ)
      case _ => ()
    }
    node.children.foreach(assignHoles)

    node match {
      case m: P.Declaration.Module =>
        val modTyp = Module(
          m.moduleScope.typeMap.map({
            case (_, (t, sym)) => sym -> t
          }).toMap
        )
        val symbol = m.scope.typeMap.get(m.ident.name) match {
          case Some((ExistentialInstance(i, _), sym)) =>
            _generics.update(i, modTyp)
            sym
        }
        m.scope.setType(m.ident.name, symbol, modTyp)
      case _ => ()
    }
  }



  private def getErrors[N](errors: ListBuffer[Diagnostic], node: T.Node): Unit = {
    applyEnv(node.meta.typ, Some(errors))
    node.children.foreach(child => getErrors(errors, child))
  }


  def applyEnv(t: Type, errors: Option[ListBuffer[Diagnostic]] = None): Type = {
    t match {
      case ExistentialInstance(i, _) =>
        _generics.get(i) match {
          case Some(ty) =>
            if (ty != t) {
              applyEnv(ty)
            } else {
              t
            }
          case None => t
        }
      case v@Var(symbol) =>
        getTypeVar(symbol).map(t => applyEnv(t, errors)).getOrElse(v)
      case Module(values) =>
        Module(
          values.mapValues(ty => {
            applyEnv(ty, errors)
          })
        )
      case _ => t
    }
  }

  private def occursIn(symbol: Symbol, typ: Type): Boolean = {
    typ match {
      case Var(s) if s == symbol => true
      case Var(_) => false
      case Constructor(_, _) | Uninferred | ErrorType => false
      case Func(label, from, to) =>
        occursIn(symbol, from) ||
          occursIn(symbol, to)
      case TApplication(f, arg) =>
        occursIn(symbol, f) || occursIn(symbol, arg)
      case Module(values) =>
        values.values.exists(t => occursIn(symbol, t))
      case Forall(_, t) => occursIn(symbol, t)
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
              case Module(values) =>
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

  def assignTypeToTVar(loc: Loc, symbol: Symbol, typ: Type): Iterable[Diagnostic] = {
    if (occursIn(symbol, typ)) {
      Some(
        Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = CyclicType
        )
      )
    } else {
      _typeVars.update(symbol, typ)
      None
    }
  }


  def setTypeVar(symbol: Symbol, typ: Type): Unit = {
    _typeVars .update(symbol, typ)
  }

  def getTypeVar(symbol: Symbol): Option[Type] = {
    _typeVars.get(symbol)
  }

  def getKindOfSymbol(symbol: Symbol): Option[Kind] = {
    _symbolKinds.get(symbol)
  }

  def setKindOfSymbol(symbol: Symbol, kind: Kind): Unit = {
    _symbolKinds.update(symbol, kind)
  }
}

