package lmc

import scala.collection.mutable
import lmc.syntax._
import lmc.common.{Loc, ScopeEntry, Symbol}
import lmc.diagnostics.{Diagnostic, Severity, TypeMismatch}
import lmc.types._

final class TypeChecker(
  private val compiler: Compiler,
  private val _setTypeOfSymbol: (Symbol, Type) => Unit,
  private val setKindOfSymbol: (Symbol, Kind) => Unit,
  private val makeUninferred: (() => Type)
) {
  import lmc.syntax.{Named => N, Typed => T}

  private val types = {
    val t = mutable.HashMap.empty[Symbol, Type]
    for ((_, ScopeEntry(_, symbol, typ)) <- compiler.PrimitivesScope.symbols) {
      t.put(symbol, typ)
    }
    t
  }

  private val typeVars = {
    val t = mutable.HashMap.empty[Symbol, Type]
    for ((_, (sym, typ, _)) <- compiler.PrimitivesScope.typeSymbols) {
      t.put(sym, typ)
    }
    t
  }

  def checkSourceFile(parsed: Parsed.SourceFile): T.SourceFile = {
    val named = new Renamer(
      (text) => compiler.makeSymbol(text),
      () => this.makeUninferred(),
      compiler.PrimitivesScope
    ).renameSourceFile(parsed)
    val declarations = checkModule(parsed.loc, named.declarations)
    T.SourceFile(
      meta = named.meta.typed,
      declarations = declarations,
      scope = named.scope
    )
  }

  private def checkModule(loc: Loc, declarations: Iterable[N.Declaration]): Iterable[T.Declaration] = {
    declarations.map(checkDeclaration)
  }

  private def checkDeclaration(declaration: N.Declaration): T.Declaration = {
    val variant = declaration.variant match {
      case N.Declaration.Let(pattern, expr) =>
        val (checkedPattern, checkedExpr) = bindExprToPattern(pattern, expr)
        T.Declaration.Let(pattern = checkedPattern, rhs = checkedExpr)
      case N.Declaration.Error() =>
        T.Declaration.Error()
    }
    T.Declaration(meta = declaration.meta.typed, variant = variant)
  }

  private def inferExpr(expr: N.Expr): T.Expr = {
    import Named.{Expr => NE}
    import Typed.{Expr => TE}
    val (variant: TE.Variant, typ: lmc.types.Type, diagnostics) = expr.variant match {
     case NE.Literal(NE.LInt(l)) =>
       (TE.Literal(TE.LInt(l)), Primitive.Int, List())
     case NE.Var(ident) =>
       val typ = getSymbolType(ident.name)
       (TE.Var(T.Ident(ident.meta.typed, ident.name)), typ, List.empty)
     case (NE.Func(tok, scope, params, returnTypeAnnotation, body)) =>
       val checkedParams = params.map((param) => {
         val namedPattern = inferPatternAndAddToScope(param.pattern)
         TE.Param(namedPattern)
       })
       val typedBody = returnTypeAnnotation match {
         case Some(annot) =>
           val annotatedType = annotationToType(checkAnnotation(annot))
           checkExpr(body, annotatedType)
         case None =>
           inferExpr(body)
       }
       val typedVariant: TE.Variant = TE.Func(tok, scope, checkedParams, None, typedBody)
       val typeFrom =
         checkedParams.map(
           (param) => (getParamLabel(param), param.pattern.typ)
         ).toList
       val typ: Type = lmc.types.Func(from = typeFrom, to = typedBody.typ)
       (
         typedVariant,
         typ,
         List.empty
       )
      case NE.Error() => (TE.Error(), ErrorType, List.empty)
    }
    T.Expr(
      meta =
        expr.meta.copy(
          diagnostics = expr.meta.diagnostics ++ diagnostics
        ).typed,
      typ = typ,
      variant = variant
    )
  }

  def getParamLabel(param: Typed.Expr.Param): Option[Symbol] = {
    getPatternLabel(param.pattern)
  }

  def getPatternLabel(pattern: Typed.Pattern): Option[Symbol] = {
    pattern.variant match {
      case T.Pattern.Var(ident) =>
          Some(ident.name)
      case T.Pattern.Annotated(inner, _) =>
          getPatternLabel(inner)
      case _ => None
    }
  }

  private def inferPatternAndAddToScope(pattern: N.Pattern): T.Pattern = {
    val (variant: T.Pattern.Variant, typ: Type, diagnostics: Iterable[Diagnostic]) =
      pattern.variant match {
        case N.Pattern.Var(ident) =>
          val checkedIdent = this.identToTypedIdent(ident)
          val variant = T.Pattern.Var(checkedIdent)
          this.resolveSymbolType(checkedIdent.name) match {
            case Some(typ) =>
              println(("HERE", typ))
              this.bindTypeToIdent(ident, typ)
              (variant, typ, List.empty)
            case None =>
              val typ = lmc.types.ErrorType
              this.bindTypeToIdent(ident, typ)
              (variant, typ, List(

              ))
          }
        case N.Pattern.Annotated(innerPattern, annotation) =>
          val checkedAnnotation: T.TypeAnnotation = checkAnnotation(annotation)
          val expectedType: lmc.types.Type = annotationToType(checkedAnnotation)
          val checkedInnerPattern: T.Pattern = checkPattern(innerPattern, expectedType)
          (T.Pattern.Annotated(
            checkedInnerPattern,
            checkedAnnotation
          ), expectedType, List())
        case N.Pattern.Error =>
          (T.Pattern.Error, lmc.types.ErrorType, List())
      }
    T.Pattern(
      meta = pattern.meta.copy(
        diagnostics = pattern.meta.diagnostics ++ diagnostics
      ).typed,
      typ = typ,
      variant = variant
    )
  }

  private def getSymbolType(symbol: Symbol): Type = {
    resolveSymbolType(symbol).getOrElse(ErrorType)
  }

  private def resolveSymbolType(symbol: Symbol): Option[Type] = {
    types.get(symbol)
  }

  private def bindExprToPattern(pattern: N.Pattern, expr: N.Expr): (T.Pattern, T.Expr) = {
    val (variant: T.Pattern.Variant, checkedExpr: T.Expr,diagnostics) = pattern.variant match {
      case N.Pattern.Var(ident) =>
        val checkedExpr = inferExpr(expr)
        val typ = checkedExpr.typ
        val newIdent = bindTypeToIdent(ident, typ)
        (T.Pattern.Var(newIdent), checkedExpr, List())
      case N.Pattern.Annotated(p, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation)
        val typ = annotationToType(checkedAnnotation)
        val checkedPattern = checkPattern(p, typ)
        val checkedExpr = checkExpr(expr, typ)
        (T.Pattern.Annotated(
          checkedPattern,
          checkedAnnotation
        ), checkedExpr, List())
      case N.Pattern.Error =>
        (T.Pattern.Error, inferExpr(expr), List())
    }
    (
      T.Pattern(
        meta =
          pattern.meta.copy(
            diagnostics = pattern.meta.diagnostics ++ diagnostics
          ).typed,
        typ = checkedExpr.typ,
        variant = variant
      ),
      checkedExpr
    )
  }

  private def checkPattern(pattern: N.Pattern, typ: Type): T.Pattern = {
    val (checkedVariant, diagnostics) = pattern.variant match {
      case N.Pattern.Var(ident) =>
        bindTypeToIdent(ident, typ)
        (
          T.Pattern.Var(T.Ident(ident.meta.typed, ident.name)),
          List.empty
        )
      case N.Pattern.Annotated(p, annotation) =>
        val checkedAnnotation = checkAnnotation(annotation)
        val annotationType = annotationToType(checkedAnnotation)
        val diagnostics =
          if (typeMoreGeneralThan(typ, annotationType)) {
             List.empty
          }
          else {
            List(
              Diagnostic(
                loc = pattern.loc,
                severity = Severity.Error,
                variant = TypeMismatch(typ, annotationType)
              )
            )
          }
        val innerPattern = checkPattern(p, annotationType)
        (
          T.Pattern.Annotated(innerPattern, checkedAnnotation),
          diagnostics
        )
      case N.Pattern.Error =>
        (T.Pattern.Error, List.empty)
    }
    T.Pattern(
      meta = pattern.meta.copy(
        diagnostics = pattern.meta.diagnostics ++ diagnostics
      ).typed,
      variant = checkedVariant,
      typ = typ
    )
  }

  private def checkExpr(expr: N.Expr, typ: Type): T.Expr = {
    val (variant: T.Expr.Variant, diagnostics: Iterable[Diagnostic]) = expr.variant match {
      case N.Expr.Literal(N.Expr.LInt(x)) =>
        val exprTyp = Primitive.Int
        val diagnostics = assertTypeMoreGeneralThan(expr.loc)(typ, exprTyp)
        (T.Expr.Literal(T.Expr.LInt(x)), diagnostics)
      case N.Expr.Var(ident) =>
        val varTyp = getSymbolType(ident.name)
        val diagnostics = assertTypeMoreGeneralThan(loc = ident.loc)(typ, varTyp)
        (T.Expr.Var(ident = T.Ident(meta = ident.meta.typed, ident.name)), diagnostics)
      case N.Expr.Func(tok, sc, namedParams, retTyp, body) =>
        typ match {
          case Func(expectedParamTypesWithLabels, expectedRetTyp) =>
            val errors = mutable.ListBuffer.empty[Diagnostic]
            val checkedParams = checkParamLists(errors)(
              expectedParamTypesWithLabels.map(_._2),
              namedParams)
            var checkedRetTyp: Option[Type] = None
            val checkedRetTypeAnnotation = retTyp.map((annot) => {
              val checkedAnnot: T.TypeAnnotation = this.checkAnnotation(annot)
              val typ: Type = this.annotationToType(checkedAnnot)
              checkedRetTyp = Some(typ)
              val errs = if (!typeMoreGeneralThan(expectedRetTyp, typ)) {
                List(Diagnostic(
                    loc = annot.loc,
                    severity = Severity.Error,
                    variant = TypeMismatch(
                      expectedRetTyp,
                      typ
                    )
                ))
              } else {
                List.empty
              }
              checkedAnnot.copy(
                meta = checkedAnnot.meta.copy(
                  diagnostics = checkedAnnot.meta.diagnostics ++ errs
                )
              )
            })
            val checkedBody = checkExpr(body, checkedRetTyp.getOrElse(expectedRetTyp))
            (T.Expr.Func(
              tok,
              sc,
              checkedParams,
              checkedRetTypeAnnotation,
              checkedBody
            ), errors)
          case _ =>
            val typedParams = namedParams.map(inferParam)
            val typedParamTypes = typedParams.map((param) => {
              param.pattern.typ
            })
            val typedParamTypesWithLabels = typedParams.zip(typedParamTypes)
              .map((tuple) => {
                val (param, typ) = tuple
                (getParamLabel(param), typ)
              }).toList
            val checkedAnnotation =
              retTyp
              .map(checkAnnotation)
            val checkedBody = inferExpr(body)
            (
              T.Expr.Func(tok, sc, typedParams, checkedAnnotation, checkedBody),
              List(Diagnostic(
                loc = checkedBody.loc,
                severity = Severity.Error,
                variant = TypeMismatch(typ, Func(typedParamTypesWithLabels, checkedBody.typ))
              ))
            )
        }
      case N.Expr.Error() =>
        (T.Expr.Error(), List.empty)
    }
    T.Expr(
      meta = expr.meta.copy(
        diagnostics = expr.meta.diagnostics ++ diagnostics
      ).typed,
      variant = variant,
      typ = typ
    )
  }

  private def checkParamLists(errors: mutable.ListBuffer[Diagnostic])
    (expected: List[Type], namedParams: Iterable[N.Expr.Param]): List[T.Expr.Param] = {
    List()
  }

  private def inferParam(param: N.Expr.Param): T.Expr.Param = {
    val typedPattern = inferPatternAndAddToScope(param.pattern)
    T.Expr.Param(typedPattern)
  }

  private def assertTypeMoreGeneralThan(loc: Loc)(t1: Type,  t2: Type): Iterable[Diagnostic] = {
    if (typeMoreGeneralThan(t1, t2)) {
      List.empty
    } else {
      List(
        Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = TypeMismatch(t1, t2)
        )
      )
    }
  }

  private def typeMoreGeneralThan(t1: Type, t2: Type): Boolean = {
    val t1Instance = instantiate(t1)
    t1Instance == t2
  }

  private def instantiate(typ: Type): Type = {
    typ
  }

  private def identToTypedIdent(ident: N.Ident): T.Ident = {
    T.Ident(meta = ident.meta.typed, ident.name)
  }

  private def checkAnnotation(annotation: Named.TypeAnnotation): T.TypeAnnotation = {
    val typedVariant = annotation.variant match {
      case N.TypeAnnotation.Var(ident) =>
        T.TypeAnnotation.Var(
          T.Ident(meta = ident.meta.typed, ident.name)
        )
      case N.TypeAnnotation.Func(params, typ) =>
        T.TypeAnnotation.Func(
          params.map((param) => {
            val (label, annotation) = param
            val checkedAnnotation = this.checkAnnotation(annotation)
            val checkedLabel = label.map(identToTypedIdent)
            (checkedLabel, checkedAnnotation)
          }),
          this.checkAnnotation(typ)
        )
      case N.TypeAnnotation.Error =>
        T.TypeAnnotation.Error
    }
    T.TypeAnnotation(
      meta = annotation.meta.typed,
      variant = typedVariant
    )
  }

  private def annotationToType(annotation: T.TypeAnnotation): Type = {
    annotation.variant match {
      case T.TypeAnnotation.Var(ident) =>
        typeVars.get(ident.name) match {
          case Some(typ) =>
            typ
          case None =>
            lmc.types.Var(ident.name)
        }
      case T.TypeAnnotation.Func(params, ret) => {
        val paramTypes = params.map((param) => {
          val (label, annot) = param
          (label.map(_.name), annotationToType(annot))
        })
        val retTyp = annotationToType(ret)
        lmc.types.Func(paramTypes.toList, retTyp)
      }
      case T.TypeAnnotation.Error => ErrorType
    }
  }

  private def bindTypeToIdent(ident: N.Ident, typ: Type): T.Ident = {
    types += ident.name -> typ
    _setTypeOfSymbol(ident.name, typ)
    T.Ident(ident.meta.typed, ident.name)
  }
}