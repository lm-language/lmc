package lmc

import java.nio.file.Paths

import lmc.syntax.{Named => N, Parsed => P}
import lmc.types._
import lmc.common.{Loc, ScopeBuilder, ScopeEntry, Symbol}
import lmc.diagnostics._

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

class Renamer(
  ctx: Context.Renamer
) {
  var _scopes = List.empty[ScopeBuilder]
  def renameSourceFile(_sourceFile: P.SourceFile): N.SourceFile = {
    val boundSourceFile =
      new Binder(ctx).bindSourceFile(_sourceFile)
    withScope(boundSourceFile.scope)(() => {
      val decls = renameModuleDecls(boundSourceFile.declarations)
      N.SourceFile(
        meta = boundSourceFile.meta.named,
        scope = boundSourceFile.scope,
        declarations = decls
      )
    })
  }

  def renameModuleDecls(declarations: Iterable[P.Declaration]): Iterable[N.Declaration] = {
    declarations
      .map(renameDecl)
  }

  def withScope[T](scope: ScopeBuilder)(f: () => T): T = {
    this._openScope(scope)
    val result = f()
    this._closeScope()
    result
  }

  def renameDecl(decl: P.Declaration): N.Declaration = {
    val (variant, diagnostics) = decl.variant match {
      case P.Declaration.Let(pattern, expr) =>
        val namedExpr = expr.map(renameExpr)
        val namedPattern = pattern.variant match {
          case (
            P.Pattern.Function(_, _)
            | P.Pattern.DotName(_)
            ) =>
            val typ = namedExpr.map(ctx.getTypedExpr).map(_.typ)
            renamePattern(pattern, None, typ)
          case _ =>
            renamePattern(pattern, None, None)
        }
        (N.Declaration.Let(namedPattern, namedExpr), List.empty)
      case P.Declaration.TypeAlias(ident, kindAnnotation, annotation) =>
        val namedIdent = renameTypeIdent(ident)
        val namedAnnotation = annotation.map(renameAnnotation)
        (N.Declaration.TypeAlias(
          namedIdent,
          kindAnnotation.map(renameKindAnnotation),
          namedAnnotation
        ), List.empty)
      case P.Declaration.Include(e) =>
        val namedExpr = renameExpr(e)
        val checkedExpr = ctx.getTypedExpr(namedExpr)
        // we can detect here if someone tried to include
        // something that's not a module but it seems like
        // a type error so we don't add it here.
        // Type checker should add that.
        checkedExpr.typ match {
          case lmc.types.Module(types, values) =>
            for ((name, _) <- types) {
              _scopes.head.setTypeVar(
                name.text,
                lmc.common.TypeEntry(name)
              )
            }
            for ((name, _) <- values) {
              _scopes.head.setSymbol(
                name.text,
                lmc.common.ScopeEntry(name)
              )
            }
          case _ => ()
        }
        (N.Declaration.Include(namedExpr), List.empty)
      case P.Declaration.Enum(scope, ident, genericParams, cases) =>
        val namedIdent = renameVarIdent(ident)
        withScope(scope)(() => {
          val namedGenericParams = renameGenericParams(genericParams).toVector
          val namedCases = cases.map(c => {
            val caseName = renameVarIdent(c.name)
            N.EnumCase(
              c.meta.named,
              caseName,
              c.args.map({
                case (label, annotation) =>
                  (label, renameAnnotation(annotation))
              })
            )
          })
          ctx.setEnumVariants(namedIdent.name, namedCases.map(_.name.name))
          (
            N.Declaration.Enum(
              scope, namedIdent, namedGenericParams, namedCases
            ),
            List.empty
          )
        })
      case P.Declaration.Error() =>
        (N.Declaration.Error(), List.empty)
    }
    val result = N.Declaration(
      meta = decl.meta.copy(
        diagnostics = decl.meta.diagnostics ++ diagnostics
      ).named,
      variant = variant,
      decl.modifiers.map(P.Declaration.Modifier.named)
    )
    addDeclToScope(result)
    result
  }

  private def addDeclToScope(declaration: N.Declaration): Unit = {
    declaration.variant match {
      case N.Declaration.Let(pattern, _) =>
        addPatternDeclToScope(pattern, declaration)
      case N.Declaration.TypeAlias(ident, _, _) =>
        addDecl(ident.name, WeakReference(declaration))
        ctx.setDeclOfTypeSymbol(ident.name, declaration)
      case N.Declaration.Enum(_, ident, _, _) =>
        ctx.setDeclOfSymbol(ident.name, declaration)
      case _ => ()
    }
  }


  private def addPatternDeclToScope(pattern: N.Pattern, declaration: N.Declaration): Unit = {
    import N.{Pattern => P}
    pattern.variant match {
      case P.Var(ident) =>
        ctx.setDeclOfSymbol(ident.name, declaration)
      case P.Annotated(inner, _) =>
        addPatternDeclToScope(inner, declaration)
      case P.Function(p, params) =>
        params.foreach({
          case P.Param.SubPattern(_, _, p) =>
            addPatternDeclToScope(p, declaration)
          case P.Param.Rest(_) => ()
        })
      case P.Error =>
        ()
    }
  }

  private def getEnumTypeSymbol(typ: Type): Option[Symbol] = {
    typ match {
      case Constructor(sym, _) => Some(sym)
      case TApplication(t, _) => getEnumTypeSymbol(t)
      case _ => None
    }
  }

  private def renamePattern(
    pattern: P.Pattern,
    annotation: Option[P.TypeAnnotation] = None,
    typ: Option[lmc.types.Type] = None
  ): N.Pattern = {
    val (namedVariant, diagnostics: Iterable[Diagnostic]) = pattern.variant match {
      case P.Pattern.Var(ident) =>
        val namedIdent = renameVariableIdent(ident)
        (N.Pattern.Var(namedIdent), List.empty)
      case P.Pattern.Annotated(pat, annotation) =>
        val newPattern = renamePattern(pat, Some(annotation))
        (N.Pattern.Annotated(
          newPattern,
          renameAnnotation(annotation)
        ), List.empty)
      case P.Pattern.DotName(ident) =>
        typ.flatMap(getEnumTypeSymbol) match {
          case Some(symbol) =>
            ctx.getEnumVariants(symbol).find(_.text == ident.name) match {
              case Some(sym) =>
                (N.Pattern.DotName(
                  N.Ident(
                    meta = ident.meta.named,
                    name = sym,
                    duplicateBinder = ident.duplicateBinder)
                ), List.empty)
              case None =>
                val renamedIdent = renameVarIdent(ident)
                (
                  N.Pattern.DotName(renamedIdent), List(Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = NoSuchVariant
                )))
            }
          case None =>
            val renamedIdent = renameVarIdent(ident)
            (N.Pattern.DotName(renamedIdent), List(Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = NoSuchVariant
            )))
        }
      case P.Pattern.Function(pf@P.Pattern(_, _, P.Pattern.DotName(dotIdent)), params) =>
        val namedPf = renamePattern(pf, None, typ)
        var i = 0
        val namedParams = params.map(param => {
          val result = param match {
            case P.Pattern.Param.Rest(meta) => N.Pattern.Param.Rest(meta.named)
            case subPattern@P.Pattern.Param.SubPattern(meta, label, p) =>
              N.Pattern.Param.SubPattern(
                meta.named,
                label.map(renameVariableIdent),
                renamePattern(p, None)
              )
          }
          i += 1
          result
        })
        (N.Pattern.Function(namedPf, namedParams), List.empty)
      case P.Pattern.Function(pf, params) =>
        // TODO: Add error here
        val namedPf = renamePattern(pf)
        val namedParams = params.map({
          case P.Pattern.Param.Rest(meta) => N.Pattern.Param.Rest(meta.named)
          case P.Pattern.Param.SubPattern(meta, label, p) =>
            N.Pattern.Param.SubPattern(
              meta.named,
              label.map(renameVarIdent),
              renamePattern(p)
            )
        })
        (N.Pattern.Function(namedPf, namedParams), List.empty)
      case P.Pattern.Error =>
        (N.Pattern.Error, List.empty[Diagnostic])
    }
    val parsedDiagnostics = pattern.meta.diagnostics
    val namedDiagnostics = parsedDiagnostics ++ diagnostics
    N.Pattern(
      meta = pattern.meta.copy(
        diagnostics = namedDiagnostics
      ).named,
      typ = (),
      variant = namedVariant
    )
  }

  private def renameAnnotation(annotation: P.TypeAnnotation): N.TypeAnnotation = {
    val errors = ListBuffer.empty[Diagnostic]
    val variant: N.TypeAnnotation.Variant = annotation.variant match {
      case P.TypeAnnotation.Var(ident) =>
        resolveTypeVar(ident.name) match {
          case Some(tVar) =>
            val namedIdent = N.Ident(meta = ident.meta.named, tVar)
            N.TypeAnnotation.Var(namedIdent)
          case None =>
            errors.append(
              Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UnBoundTypeVar(ident.name)
                )
            )
            N.TypeAnnotation.Var(
              N.Ident(meta = ident.meta.named, ctx.makeSymbol(ident.name))
            )
        }
      case P.TypeAnnotation.Func(params, returnType) =>
        val namedParams = params.map(renameAnnotationParam)
        val namedReturnType = renameAnnotation(returnType)
        N.TypeAnnotation.Func(namedParams, namedReturnType)
      case P.TypeAnnotation.Forall(scope, params, typeAnnotation) =>
        withScope(scope)(() => {
          val namedParams = renameGenericParams(params)
          val namedAnnotation = renameAnnotation(typeAnnotation)
          N.TypeAnnotation.Forall(scope, namedParams, namedAnnotation)
        })
      case P.TypeAnnotation.TApplication(f, args) =>
        N.TypeAnnotation.TApplication(
          renameAnnotation(f),
          args.map(renameAnnotation)
        )
      case P.TypeAnnotation.Prop(e, prop) =>
        N.TypeAnnotation.Prop(
          renameExpr(e),
          prop
        )
      case P.TypeAnnotation.Error =>
        N.TypeAnnotation.Error
    }
    N.TypeAnnotation(
      meta = annotation.meta.copy(
        diagnostics = annotation.meta.diagnostics ++ errors.toVector
      ).named,
      (),
      variant = variant
    )
  }

  private def renameGenericParams(params: Iterable[P.GenericParam]): Iterable[N.GenericParam] = {
    params.map(renameGenericParam)
  }

  private def renameGenericParam(param: P.GenericParam): N.GenericParam = {
    val renamedIdent = renameTypeIdent(param.ident)
    val renamedKindAnnotation = param.kindAnnotation.map(renameKindAnnotation)
    N.GenericParam(
      meta = param.meta.named,
      kind = (),
      ident = renamedIdent,
      kindAnnotation =  renamedKindAnnotation
    )
  }

  private def renameKindAnnotation(annotation: P.KindAnnotation): N.KindAnnotation = {
    annotation.variant match {
      case P.KindAnnotation.Star =>
        N.KindAnnotation(annotation.meta.named, N.KindAnnotation.Star)
      case P.KindAnnotation.KFun(from, to) =>
        N.KindAnnotation(
          annotation.meta.named,
          N.KindAnnotation.KFun(
            from.map(renameKindAnnotation),
            renameKindAnnotation(to)
          )
        )
      case P.KindAnnotation.Error =>
        N.KindAnnotation(annotation.meta.named, N.KindAnnotation.Error)
    }
  }

  private def renameVarIdent(ident: P.Ident): N.Ident = {
    val entryOpt = for {
      scope <- ident.meta.scope.get
      entry <- scope.resolveEntry(ident.name)
    } yield entry
    val (symbol, errors) = entryOpt match {
      case Some(entry) =>
        val errors: List[Diagnostic] = entry.validAfter match {
          case Some(pos) =>
            if (!pos.lt(ident.loc.start)) {
              List(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UseBeforeAssignment(ident.name)
                )
              )
            } else {
              List.empty
            }
          case None =>
            List.empty
        }
        (entry.symbol, errors)
      case None =>
        (ctx.makeSymbol(ident.name), List(
          Diagnostic(
            loc = ident.loc,
            severity = Severity.Error,
            variant = UnBoundVar(ident.name)
          )
        ))
    }
    N.Ident(
      ident
        .meta
        .withDiagnostics(errors)
        .named,
      symbol
    )
  }

  private def renameAnnotationParam(param: P.TypeAnnotation.Param): N.TypeAnnotation.Param = {
    val (labelOpt, typ) = param
    val renamedTyp = renameAnnotation(typ)
    val label = labelOpt map (label =>
      N.Ident(label.meta.named, ctx.makeSymbol(label.name)))
    (label, renamedTyp)
  }

  private def renameExpr(expr: P.Expr): N.Expr = {
    val namedVariant = expr.variant match {
      case P.Expr.Var(ident) =>
        val namedIdent = renameVarIdent(ident)
        N.Expr.Var(namedIdent)
      case P.Expr.Literal(P.Expr.LInt(v)) =>
        N.Expr.Literal(N.Expr.LInt(v))
      case P.Expr.Func(tok, scope, genericParams, params, annotation, body) =>
        withScope(scope)(() => {
          val namedGenericParams = genericParams.map(renameGenericParam)
          val namedParams = params.map((param) => {
            val newPattern = renamePattern(param.pattern)
            N.Expr.Param(newPattern)
          })
          val namedAnnotation = annotation.map(renameAnnotation)
          val namedBody = renameExpr(body)
          N.Expr.Func(
            tok,
            scope,
            namedGenericParams,
            namedParams,
            namedAnnotation,
            namedBody
          )
        })
      case P.Expr.Call(loc, func, args) =>
        val namedFunc = renameExpr(func)
        val namedArgs = args.map((arg) => {
          N.Expr.Arg(arg.label, renameExpr(arg.value))
        })
        N.Expr.Call(loc, namedFunc, namedArgs)
      case P.Expr.Module(scope, declarations) =>
        withScope(scope)(() => {
          N.Expr.Module(scope, renameModuleDecls(declarations))
        })
      case P.Expr.Prop(e, prop) =>
        N.Expr.Prop(
          renameExpr(e),
          prop
        )
      case P.Expr.WithExpression(e1, e2) =>
        N.Expr.WithExpression(
          renameExpr(e1),
          renameExpr(e2)
        )
      case P.Expr.Block(s, members) =>
        withScope(s)(() => {
          N.Expr.Block(
            s,
            members.map({
              case e@P.Expr(_, _, _) => renameExpr(e)
              case d@P.Declaration(_, _, _) => renameDecl(d)
            })
          )
        })
      case P.Expr.If(p, t, f) =>
        N.Expr.If(
          renameExpr(p),
          renameExpr(t),
          f.map(renameExpr)
        )
      case P.Expr.Match(e, branches) =>
        val renamedExpr = renameExpr(e)
        val checkedExpr = ctx.getTypedExpr(renamedExpr)
        val renamedBranches = branches.map(renameMatchBranch(checkedExpr.typ))
        N.Expr.Match(
          renamedExpr,
          renamedBranches
        )
      case P.Expr.Error() =>
        N.Expr.Error()
    }
    N.Expr(
      meta = expr.meta.named,
      typ = (),
      variant = namedVariant
    )
  }

  private def renameMatchBranch(typ: lmc.types.Type)(b: P.Expr.MatchBranch): N.Expr.MatchBranch = {
    withScope(b.scope)(() => {
      val lhs = renamePattern(b.lhs, None, Some(typ))
      val rhs = renameExpr(b.rhs)
      N.Expr.MatchBranch(
        b.scope,
        lhs,
        rhs
      )
    })
  }

  private def _openScope(scope: ScopeBuilder): Unit = {
    _scopes = scope::_scopes
  }

  private def _closeScope(): Unit = {
    _scopes = _scopes.tail
  }

  def resolveEntry(name: String): Option[ScopeEntry] = {
    _scopes match {
      case sc :: _ =>
        sc.resolveEntry(name)
      case Nil =>
        ctx.PrimitiveScope.getEntry(name)
    }
  }


  private def renameTypeIdent(ident: P.Ident): N.Ident = {
    _scopes.head.typeSymbols.get(ident.name) match {
      case Some(entry) =>
        N.Ident(ident.meta.named, entry.symbol, ident.duplicateBinder)
      case None =>
        throw new Error(s"Compiler bug: No typeVar symbol for $ident; Check binder")
    }
  }

  private def addDecl(symbol: Symbol, decl: WeakReference[N.Declaration]): Unit = {
    _scopes.head.addDeclaration(symbol, decl)
  }

  def renameVariableIdent(ident: P.Ident): N.Ident = {
    _scopes.head.symbols.get(ident.name) match {
      case Some(entry) =>
        N.Ident(ident.meta.named, entry.symbol, ident.duplicateBinder)
      case None =>
        throw new Error(
          s"Compiler bug: No var symbol for $ident; Check binder; ${ident.loc}"
        )
    }
  }

  def resolveTypeVar(name: String): Option[Symbol] = {
    _scopes match {
      case sc::_ =>
        sc.resolveTypeEntry(name).map(_.symbol)
      case Nil =>
        ctx.PrimitiveScope.resolveTypeEntry(name).map(_.symbol)
    }


  }
}
