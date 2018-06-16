package lmc

import lmc.common.{Loc, ScopeEntry, Symbol, TypeEntry}
import lmc.diagnostics._
import lmc.types._
import lmc.utils.Debug

import scala.collection.mutable.ListBuffer
import scala.collection.mutable


final class TypeChecker(
  private val ctx: Context.TC
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private val Primitive = ctx.Primitive

  private val _checkedDecls = collection.mutable.WeakHashMap.empty[Int, T.Declaration]

  private val _symbolType = collection.mutable.WeakHashMap.empty[Symbol, Type]
  private val _symbolKind = collection.mutable.WeakHashMap.empty[Symbol, Kind]
  private val _typeVars =
    collection.mutable.WeakHashMap.empty[Symbol, Type]

  private val _genericInstances = collection.mutable.WeakHashMap.empty[Int, Type]

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    val inferredDeclarations = sourceFile.declarations.map(inferDeclaration)
    T.SourceFile(
      sourceFile.meta.typed(getModuleTypeFromDeclarations(inferredDeclarations)),
      sourceFile.scope,
      inferredDeclarations
    )
  }

  def getModuleTypeFromDeclarations(declarations: Array[T.Declaration]): Type = {
    val types = mutable.HashMap.empty[Symbol, Kind]
    val values = mutable.HashMap.empty[Symbol, Type]

    def addPatternBindings(p: T.Pattern): Unit = {
      p match {
        case v: T.Pattern.Var => values.update(v.ident.name, v.meta.typ)
        case a: T.Pattern.Annotated => addPatternBindings(a.innerPattern)
        case _: T.Pattern.Error => ()
        case p: T.Pattern.Paren => addPatternBindings(p.inner)
        case f: T.Pattern.Function =>
          f.params.foreach({
            case _: T.Pattern.Param.Rest => ()
            case s: T.Pattern.Param.SubPattern =>
              addPatternBindings(s.pattern)
          })

      }
    }

    for (decl <- declarations) {
      decl match {
        case d: T.Declaration.Let =>
          addPatternBindings(d.pattern)
        case d: T.Declaration.TypeAlias =>
          getKindOfSymbol(d.ident.name) match {
            case Some(k) =>
              types.update(d.ident.name, k)
            case None =>
              throw new Error(s"Compiler bug: No kind for symbol ${d.ident.name}")
          }
        case _ => ()
      }
    }
    val result = Module(
      types.toMap,
      values.toMap
    )

    result
  }


  def inferDeclaration(decl: P.Declaration): T.Declaration = {
    _checkedDecls.get(decl.meta.id) match {
      case Some(d) => d
      case None =>
        val inferred = inferDeclarationWorker(decl)
        _checkedDecls.update(decl.meta.id, inferred)
        inferred
    }
  }

  def inferDeclarationWorker(decl: P.Declaration): T.Declaration = {
    import P.{Declaration => PD}
    import T.{Declaration => TD}
    val errors = ListBuffer.empty[Diagnostic]
    checkModifiers(errors)(decl.modifiers)
    decl match {
      case PD.Let(
        meta,
        modifiers,
        P.Pattern.Annotated(pMeta, innerPattern, annotation),
        Some(rhs)
      ) =>
        val inferredAnnotation = inferTypeAnnotation(annotation)
        val typ = inferredAnnotation.meta.typ
        val checkedInnerPattern = checkPattern(typ)(innerPattern)
        val checkedPattern = T.Pattern.Annotated(
          pMeta.typed(typ), checkedInnerPattern, inferredAnnotation
        )
        val checkedRhs = checkExpr(typ)(rhs)
        TD.Let(
          meta.typed(typ).withDiagnostics(errors),
          modifiers.map(_.typed),
          checkedPattern,
          Some(checkedRhs)
        )
      case PD.Let(meta, modifiers, pattern, Some(rhs)) =>
        checkModifiers(errors)(modifiers)
        val inferredRhs = inferExpr(rhs)
        val checkedPattern = checkPattern(inferredRhs.meta.typ)(pattern)
        TD.Let(
          meta.typed(Primitive.Unit).withDiagnostics(errors),
          modifiers.map(_.typed),
          checkedPattern,
          Some(inferredRhs)
        )
      case PD.Let(meta, modifiers, pattern, None) =>
        checkModifiers(errors)(modifiers)
        val inferredPattern = inferPattern(pattern)
        TD.Let(
          meta.typed(Primitive.Unit).withDiagnostics(errors),
          modifiers.map(_.typed),
          inferredPattern,
          None
        )
      case PD.TypeAlias(meta, modifiers, ident, kindAnnotation, rhs) =>
        checkModifiers(errors)(modifiers)
        val checkedKindAnnotation = kindAnnotation.map(inferKindAnnotation)
        val kind = checkedKindAnnotation match {
          case Some(kindAnnotation) =>
            getKindFromAnnotation(kindAnnotation)
          case None =>
            Kind.Star
        }

        val inferredIdent = inferTypeVarBindingIdent(ident)
        val typedRhs = rhs.map(inferTypeAnnotation)
        val typ = typedRhs match {
          case Some(r) =>
            r.meta.typ
          case None =>
            Var(inferredIdent.name)
        }
        setTypeVar(inferredIdent.name, typ)
        setKindOfSymbol(inferredIdent.name, kind)
        decl.scope.addDeclaration(inferredIdent.name, meta.id)
        TD.TypeAlias(
          meta.typed(Primitive.Unit).withDiagnostics(errors),
          modifiers.map(_.typed),
          inferredIdent,
          checkedKindAnnotation,
          typedRhs
        )
      case PD.Error(meta, modifiers) =>
        TD.Error(
          meta.typed(Primitive.Unit),
          modifiers.map(_.typed)
        )

    }
  }


  def setKindOfSymbol(name: Symbol, kind: Kind) = {
    _symbolKind.update(name, kind)
  }

  def getKindOfSymbol(name: Symbol): Option[Kind] = {
    _symbolKind.get(name)
  }

  private def getKindFromAnnotation(annotation: T.KindAnnotation): Kind = {
    annotation match {
      case _: T.KindAnnotation.Star => Kind.Star
      case k: T.KindAnnotation.Func =>
        k.from.foldRight(getKindFromAnnotation(k.to))((current, previous) =>
          Kind.KFun(getKindFromAnnotation(current), previous))
      case _: T.KindAnnotation.Error => Kind.Star
    }
  }

  def inferKindAnnotation(annot: P.KindAnnotation): T.KindAnnotation = {
    annot match {
      case P.KindAnnotation.Star(meta) =>
        T.KindAnnotation.Star(meta.typed(Primitive.Unit))
      case P.KindAnnotation.Func(meta, from, to) =>
        T.KindAnnotation.Func(
          meta.typed(Primitive.Unit),
          from.map(inferKindAnnotation),
          inferKindAnnotation(to)
        )
      case P.KindAnnotation.Error(meta) =>
        T.KindAnnotation.Star(meta.typed(Primitive.Unit))
    }
  }

  def inferExpr(expr: P.Expression): T.Expression = {
    expr match {
      case P.Expression.Literal(meta, P.Expression.Literal.LInt(value)) =>
        T.Expression.Literal(
          meta.typed(Primitive.Int),
          T.Expression.Literal.LInt(value)
        )
      case P.Expression.Var(meta, ident) =>
        val inferredIdent = inferVarIdent(ident)
        T.Expression.Var(meta.typed(inferredIdent.meta.typ), inferredIdent)
      case call: P.Expression.Call =>
        inferCall(call)
      case func: P.Expression.Func =>
        inferFunc(func)
      case mod: P.Expression.Module =>
        inferModule(mod)
      case prop: P.Expression.Prop =>
        inferProp(prop)
      case P.Expression.Error(meta) =>
        T.Expression.Error(
          meta.typed(Uninferred)
        )
    }
  }

  def inferProp(prop: P.Expression.Prop): T.Expression.Prop = {
    val inferredExpr = inferExpr(prop.expr)
    inferredExpr.meta.typ match {
      case modTyp: Module =>
        modTyp.symbolOfString.get(prop.prop.name) match {
          case Some(symbol) =>
            getTypeOfSymbol(symbol) match {
              case Some(t) =>
                T.Expression.Prop(
                  prop.meta.typed(t),
                  inferredExpr,
                  T.Ident(
                    prop.prop.meta.typed(t),
                    symbol
                  )
                )
              case None =>
                throw new Error(s"Compiler bug: No type for symbol ${symbol}")
            }
          case None =>
            T.Expression.Prop(
              meta = prop.meta.typed(Uninferred),
              inferredExpr,
              T.Ident(
                prop.prop.meta.typed(Uninferred).withDiagnostic(
                  Diagnostic(
                    loc = prop.prop.loc,
                    severity = Severity.Error,
                    variant = NoSuchValueProperty(prop.prop.name)
                  )
                ),
                ctx.makeSymbol(prop.prop.name)
              )
            )
        }
      case typ =>
        T.Expression.Prop(
          prop.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = inferredExpr.loc,
              severity = Severity.Error,
              variant = NotAModule
            )
          ),
          inferredExpr,
          T.Ident(
            prop.prop.meta.typed(Uninferred),
            ctx.makeSymbol(prop.prop.name)
          )
        )
    }
  }


  def inferPropAnnotation(prop: P.TypeAnnotation.Prop): T.TypeAnnotation.Prop = {
    val inferredExpr = inferExpr(prop.expr)
    inferredExpr.meta.typ match {
      case modTyp: Module =>
        modTyp.symbolOfString.get(prop.prop.name) match {
          case Some(symbol) =>
            getKindOfSymbol(symbol) match {
              case Some(t) =>
                T.TypeAnnotation.Prop(
                  prop.meta.typed(Primitive.Unit),
                  inferredExpr,
                  T.Ident(
                    prop.prop.meta.typed(Primitive.Unit),
                    symbol
                  )
                )
              case None =>
                throw new Error(s"Compiler bug: No kind for symbol $symbol")
            }
          case None =>
            T.TypeAnnotation.Prop(
              meta = prop.meta.typed(Primitive.Unit),
              inferredExpr,
              T.Ident(
                prop.prop.meta.typed(Uninferred).withDiagnostic(
                  Diagnostic(
                    loc = prop.prop.loc,
                    severity = Severity.Error,
                    variant = NoSuchTypeProperty(prop.prop.name)
                  )
                ),
                ctx.makeSymbol(prop.prop.name)
              )
            )
        }
      case typ =>
        T.TypeAnnotation.Prop(
          prop.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = inferredExpr.loc,
              severity = Severity.Error,
              variant = NotAModule
            )
          ),
          inferredExpr,
          T.Ident(
            prop.prop.meta.typed(Uninferred),
            ctx.makeSymbol(prop.prop.name)
          )
        )
    }
  }

  def inferModule(module: P.Expression.Module): T.Expression.Module = {
    val inferredDeclarations = module.declarations.map(inferDeclaration)
    val modType = getModuleTypeFromDeclarations(inferredDeclarations)
    T.Expression.Module(
      module.meta.typed(modType),
      module.moduleScope,
      inferredDeclarations
    )
  }

  def inferFunc(func: P.Expression.Func): T.Expression.Func = {
    val genericParams = inferGenericParams(func.genericParams)
    val params = func.params.map(
      p => T.Expression.Param(inferPattern(p.pattern))
    )
    val retTypAnnotationUnchecked = func.returnType.map(inferTypeAnnotation)
    val retTypeAnnotation = retTypAnnotationUnchecked match {
      case Some(retTyp) =>
        getKindOfType(retTyp.meta.typ) match {
          case Kind.Star =>
            retTypAnnotationUnchecked
          case k =>
            retTypAnnotationUnchecked.map(r => r.withMeta(
              r.meta.withDiagnostic(
                Diagnostic(
                  loc = r.loc,
                  severity = Severity.Error,
                  variant = KindMismatch(Kind.Star, k)
                )
              )
            ))

        }
      case None => None
    }
    val body = retTypeAnnotation match {
      case Some(annot) => checkExpr(annot.meta.typ)(func.body)
      case None => inferExpr(func.body)

    }
    val retTyp = body.meta.typ
    val monoType = Func(
      params.map(p => (getPatternLabel(p.pattern).map(_._1), p.pattern.meta.typ)),
      retTyp
    )

    val typ = if (genericParams.isEmpty) {
      monoType
    } else {
      genericParams.foldRight[Type](monoType)((param, body) =>
        Forall(param.ident.name, body)
      )
    }
    T.Expression.Func(
      func.meta.typed(typ),
      func.fnToken,
      func.funcScope,
      genericParams,
      params,
      retTypeAnnotation,
      body
    )
  }

  def getPatternLabel(p: T.Pattern): Option[(Symbol, Loc)] = {
    p match {
      case T.Pattern.Var(_, ident) => Some(ident.name, ident.loc)
      case T.Pattern.Annotated(_, T.Pattern.Var(_, ident), _) =>
        Some(ident.name, ident.loc)
      case _ => None
    }
  }

  def inferCall(call: P.Expression.Call): T.Expression.Call = {
    val func = inferExpr(call.func)
    val funcTyp = instantiateForall(func.meta.typ)
    funcTyp match {
      case Func(from, to) =>
        val errors = ListBuffer.empty[Diagnostic]
        val args = checkFunctionArgs(func.meta.loc, errors, from, call.args)
        val typ = resolveType(to)
        T.Expression.Call(
          call.meta.typed(typ).withDiagnostics(errors),
          func,
          args
        )
      case _ =>
        val args = call.args.map({
          case P.Expression.Call.Arg(meta, labelOpt, value) =>
            val inferredValue = inferExpr(value)
            T.Expression.Call.Arg(
              meta.typed(inferredValue.meta.typ),
              labelOpt.map(l => T.Ident(
                l.meta.typed(inferredValue.meta.typ),
                ctx.makeSymbol(l.name)
              )),
              inferredValue
            )
        })
        T.Expression.Call(
          call.meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = func.loc,
              severity = Severity.Error,
              variant = NotAFunction(funcTyp)
            )
          ),
          func,
          args
        )
    }

  }

  def resolveType(t: Type): Type = {
    t match {
      case ExistentialInstance(id, text) =>
        getGeneric(id).getOrElse(ExistentialInstance(id, text))
      case _: Var => t
      case _: Constructor => t
      case Uninferred => t
      case Module(types, values) =>
        Module(types, values.mapValues(resolveType))
      case TApplication(tFunc, tArg) =>
        TApplication(resolveType(tFunc), resolveType(tArg))
      case _ => t
    }
  }

  def checkGenericParams(
    errors: ListBuffer[Diagnostic]
  )(typ: Type, params: Array[P.GenericParam]): Array[T.GenericParam] = {
    (typ, params.length) match {
      case (_, 0) =>
        inferGenericParams(params)
      case (Forall(p, t), n) =>
        val head = inferGenericParam(params.head)
        setTypeVar(head.ident.name, Var(p))
        head +: checkGenericParams(errors)(t, params.tail)
      case (t, n) =>
        inferGenericParams(params)
    }
  }


  def checkFunc(func: P.Expression.Func, typ: Type): T.Expression.Func = {
    typ match {
      case Forall(param, typ) =>
        val errors = ListBuffer.empty[Diagnostic]
        if (func.genericParams.length == 0) {
          checkFunc(func, typ)
        } else {
          val head = inferGenericParam(func.genericParams.head)
          setTypeVar(head.ident.name, Var(param))
          setKindOfSymbol(head.ident.name, Kind.Star)
          checkFunc(func.copy(
            genericParams = func.genericParams.tail
          ), typ)
        }
      case Func(from, to) =>
        val checkedGenericParams = inferGenericParams(func.genericParams)
        val funcParamsVec = func.params.toVector
        var i = 0
        val checkedParams = ListBuffer.empty[T.Expression.Param]
        for ((expectedParamLabel, expectedParamTyp) <- from) {
          if (i < funcParamsVec.length) {
            val funcParam = funcParamsVec(i)
            var checkedPattern = checkPattern(expectedParamTyp)(funcParam.pattern)
            expectedParamLabel match {
              case Some(expectedLabelSym) =>
                val paramLabelOpt = getPatternLabel(checkedPattern)
                if (
                  !paramLabelOpt.map(_._1.text)
                    .contains(expectedLabelSym.text)
                ) {
                  val loc = paramLabelOpt.map(_._2).getOrElse(checkedPattern.loc)
                  checkedPattern = checkedPattern.withMeta(
                    checkedPattern.meta.withDiagnostic(
                      Diagnostic(
                        loc = loc,
                        severity = Severity.Error,
                        variant = FuncParamLabelMismatch(expectedLabelSym.text)
                      )
                    )
                  )
                }
              case None =>
                ()
            }
            checkedParams.append(T.Expression.Param(checkedPattern))
          }
          i += 1
        }
        while (i < funcParamsVec.length) {
          val funcParam = funcParamsVec(i)
          var inferredPattern = inferPattern(funcParam.pattern)
          inferredPattern = inferredPattern.withMeta(
            inferredPattern.meta.withDiagnostic(
              Diagnostic(
                loc = inferredPattern.loc,
                severity = Severity.Error,
                variant = ExtraParam
              )
            )
          )
          checkedParams.append(T.Expression.Param(inferredPattern))
          i += 1
        }
        val checkedRetTypAnnotation = func
          .returnType
          .map(t => { checkAnnotation(to)(t) })
        val checkedBody = checkedRetTypAnnotation match {
          case Some(annot) =>
            val t = annot.meta.typ
            checkExpr(t)(func.body)
          case None =>
            checkExpr(to)(func.body)
        }
        T.Expression.Func(
          func.meta.typed(typ),
          func.fnToken,
          func.scope,
          checkedGenericParams,
          checkedParams.toArray,
          checkedRetTypAnnotation,
          checkedBody
        )
      case _ =>
        val inferred = inferFunc(func)
        inferred.copy(
          meta = inferred.meta.withDiagnostic(
            Diagnostic(
              loc = func.loc,
              severity = Severity.Error,
              variant = TypeMismatch(
                expected = typ,
                found = inferred.meta.typ
              )
            )
          )
        )
    }
  }

  def checkAnnotation(typ: Type)(annotation: P.TypeAnnotation): T.TypeAnnotation = {
    val inferredAnnot = inferTypeAnnotation(annotation)
    val errors = ListBuffer.empty[Diagnostic]
    assertTypeMatch(errors, annotation.loc)(
      expected = typ, found = inferredAnnot.meta.typ
    )
    inferredAnnot.withMeta(
      inferredAnnot.meta.withDiagnostics(errors)
    )
  }

  def checkFunctionArgs(): Unit = {

  }

  def checkFunctionArgs(
    loc: Loc,
    errors: ListBuffer[Diagnostic],
    params: Array[(Option[Symbol], Type)],
    args: Array[P.Expression.Call.Arg]
  ): Array[T.Expression.Call.Arg] = {
    val labelChecked = mutable.Set.empty[String]
    var i = 0
    val checkedArgs = ListBuffer.empty[T.Expression.Call.Arg]
    val missingArguments = ListBuffer.empty[(Option[String], Type)]
    var labelEncountered = false
    val labeledArgsToCheck = ListBuffer.empty[(String, P.Expression.Call.Arg)]
    val expectedLabeledArgs = params.filter({ _._1.isDefined }).map(arg => {
      (arg._1.get.text, (arg._1.get -> arg._2))
    }).toMap
    for (expectedArg <- params) {
      if (i >= args.length) {
        missingArguments.append(expectedArg._1.map(_.text) -> expectedArg._2)
        expectedArg._1 match {
          case Some(s) => labelChecked += s.text
          case None => ()
        }
      } else {
        val arg = args(i)
        arg.label match {
          case Some(label) =>
            labelEncountered = true
            labeledArgsToCheck.append(label.name -> arg)
          case None =>
            val diagnostic = if (labelEncountered) {
              Some(Diagnostic(
                loc = arg.value.loc,
                severity = Severity.Error,
                variant = PositionalArgAfterLabelled
              ))
            } else {
              None
            }
            expectedArg._1 match {
              case None => ()
              case Some(label) =>
                labelChecked += label.text
            }
            var checkedExpr = checkExpr(expectedArg._2)(arg.value)
            diagnostic match {
              case Some(d) =>
                checkedExpr = checkedExpr.withMeta(
                  checkedExpr.meta.withDiagnostic(d)
                )
              case None => ()
            }
            val checkedArg = T.Expression.Call.Arg(
              arg.meta.typed(checkedExpr.meta.typ), None, checkedExpr)
            checkedArgs.append(checkedArg)
        }
      }
      i += 1
    }
    while (i < args.length) {
      val arg = args(i)
      arg.label match {
        case Some(label) =>
          labelEncountered = true
          labeledArgsToCheck.append(label.name -> arg)
        case None =>
          var inferredExpr = inferExpr(arg.value)
          inferredExpr = inferredExpr.withMeta(
            inferredExpr.meta.withDiagnostic(
              Diagnostic(
                loc = arg.value.loc,
                severity = Severity.Error,
                variant = ExtraArg
              )
            )
          )
          val checkedArg = T.Expression.Call.Arg(
            arg.meta.typed(inferredExpr.meta.typ),
            None,
            inferredExpr
          )
          checkedArgs.append(checkedArg)
      }
      i += 1
    }
    for ((name, arg) <- labeledArgsToCheck) {
      expectedLabeledArgs.get(name) match {
        case Some((expectedLabel, expectedTyp)) =>
          val diagnostic = if (labelChecked.contains(name)) {
            Some(Diagnostic(
              loc = arg.label.map(_.loc).getOrElse(arg.meta.loc),
              severity = Severity.Error,
              variant = DuplicateLabelArg(name)
            ))
          } else {
            None
          }
          var checkedExpr = checkExpr(expectedTyp)(arg.value)
          diagnostic match {
            case Some(d) =>
              checkedExpr = checkedExpr.withMeta(
                checkedExpr.meta.withDiagnostic(d)
              )
            case None => ()
          }
          val checkedLabel = arg.label.map(i => {
            T.Ident(
              meta = i.meta.typed(checkedExpr.meta.typ),
              expectedLabel
            )
          })
          val checkedArg = T.Expression.Call.Arg(
            arg.meta.typed(checkedExpr.meta.typ),
            checkedLabel,
            checkedExpr
          )
          checkedArgs.append(checkedArg)
          labelChecked += name
        case None =>
          val inferredExpr = inferExpr(arg.value)
          val inferredArg = T.Expression.Call.Arg(
            arg.meta.typed(inferredExpr.meta.typ).withDiagnostic(
              Diagnostic(
                loc = arg.label.map(_.loc).getOrElse(arg.loc),
                severity = Severity.Error,
                variant = NoSuchParamLabel(name)
              )
            ),
            label = arg.label.map(i =>
              T.Ident(
                i.meta.typed(inferredExpr.meta.typ),
                ctx.makeSymbol(i.name))
            ),
            value = inferredExpr
          )
          checkedArgs.append(inferredArg)
      }
    }
    for (expected <- params) {
      expected._1 match {
        case Some(name) =>
          if (!labelChecked.contains(name.text)) {
            missingArguments.append(Some(name.text) -> expected._2)
          }
        case None => ()
      }
    }
    if (missingArguments.nonEmpty) {
      errors.append(
        Diagnostic(
          loc = loc,
          variant = MissingArguments(missingArguments.toList),
          severity = Severity.Error
        )
      )
    }

    checkedArgs.toArray
  }



  private def instantiateForall(typ: Type): Type = {
    typ match {
      case Forall(param, innerTyp) =>
        val subst = mutable.Map.empty[Symbol, Type]
        subst.put(param, ctx.makeGenericType(param.text))
        instantiateForall(applySubst(innerTyp, subst))
      case _ => typ
    }
  }


  private def applySubst(t: Type, subst: collection.Map[Symbol, Type]): Type = {
    t match {
      case
        Constructor(_, _)
        | ErrorType
        | ExistentialInstance(_, _)
        | Uninferred
      => t
      case Var(name) =>
        subst.get(name) match {
          case Some(typ) =>
            applySubst(typ, subst)
          case None => t
        }
      case Forall(params, inner) =>
        Forall(params, applySubst(inner, subst))
      case TApplication(f, arg) =>
        TApplication(applySubst(f, subst), applySubst(arg, subst))
      case Module(types, values) =>
        Module(types, values.mapValues(typ => applySubst(typ, subst)))
      case Abstract(types, values, inner) =>
        Abstract(
          types,
          values.mapValues(t => applySubst(t, subst)),
          applySubst(inner, subst)
        )
      case Func(from, to) =>
        Func(
          from.map(param => (param._1, applySubst(param._2, subst))),
          applySubst(to, subst)
        )
    }
  }

  def inferPattern(pattern: P.Pattern): T.Pattern = {
    pattern match {
      case P.Pattern.Var(meta, ident) =>
        val inferredIdent = inferVarBindingIdent(ident, Uninferred)
        T.Pattern.Var(
          meta.typed(Uninferred).withDiagnostic(
            Diagnostic(
              loc = pattern.loc,
              severity = Severity.Error,
              variant = MissingTypeAnnotation
            )
          ),
          inferredIdent
        )
      case P.Pattern.Annotated(meta, innerPattern, typeAnnotation) =>
        val inferredAnnotation = inferTypeAnnotation(typeAnnotation)
        val checkedInnerPattern =
          checkPattern(inferredAnnotation.meta.typ)(innerPattern)
        val kind = getKindOfType(inferredAnnotation.meta.typ)
        val errors = kind match {
          case Kind.Star =>
            Array.empty[Diagnostic]
          case _ =>
            Array(Diagnostic(
              loc = typeAnnotation.loc,
              severity = Severity.Error,
              variant = KindMismatch(Kind.Star, kind)
            ))
        }
        T.Pattern.Annotated(
          meta.typed(inferredAnnotation.meta.typ).withDiagnostics(
            errors
          ),
          checkedInnerPattern,
          inferredAnnotation
        )

    }
  }

  def checkExpr(typ: Type)(expr: P.Expression): T.Expression = {
    (expr, typ) match {
      case (f: P.Expression.Func, _) =>
        checkFunc(f, typ)
      case (_, forall: Forall) =>
        checkExpr(forall.typ)(expr)
      case _ =>
        val inferredExpr = inferExpr(expr)
        val errors = ListBuffer.empty[Diagnostic]
        assertTypeMatch(
          errors, inferredExpr.loc
        )(expected = typ, found = inferredExpr.meta.typ)
        inferredExpr.withMeta(inferredExpr.meta.withDiagnostics(
          errors
        ))
    }
  }


  private def assertTypeMatch(
    errors: ListBuffer[diagnostics.Diagnostic],
    loc: Loc
  )(
    expected: Type,
    found: Type
  ): Unit = {
    (expected, found) match {
      case (ExistentialInstance(i1, _), ExistentialInstance(i2, _))
        if i1 == i2 => ()
      case _ if expected == found => ()
      case (Func(from1, to1), Func(from2, to2)) =>
        // TODO: from2 <: from1
        assertTypeMatch(errors, loc)(to1, to2)
      case (_, existential@ExistentialInstance(_, _)) =>
        instantiateR(errors, loc)(expected, existential)
      case (existential@ExistentialInstance(_, _), _) =>
        instantiateL(errors, loc)(existential, found)
      case (Forall(param, innerTyp), _) =>
        val subst = mutable.Map.empty[Symbol, Type]
        subst.put(param, ctx.makeGenericType(param.text))
        val substituted = applySubst(innerTyp, subst)
        assertTypeMatch(errors, loc)(substituted, found)
      case (_, Forall(param, innerTyp)) =>
        val subst = mutable.Map.empty[Symbol, Type]
        subst.put(param, ctx.makeGenericType(param.text))
        val substituted = applySubst(innerTyp, subst)
        assertTypeMatch(errors, loc)(expected, substituted)
      case (TApplication(aF, aArg), TApplication(bF, bArg)) =>
        assertTypeMatch(errors, loc)(aF, bF)
        // Type application is invariant; i.e.
        // List[Super] is neither a super type, nor a sub type
        // of List[Sub] if Sub <: Super
        assertTypeMatch(errors, loc)(aArg, bArg)
      case _ =>
        errors.append(
          Diagnostic(
            loc = loc,
            severity = Severity.Error,
            variant = TypeMismatch(expected, found)
          )
        )
    }
  }


  private def instantiateL(errors: ListBuffer[Diagnostic], loc: Loc)(existential: ExistentialInstance, t: Type): Unit = {
    getGeneric(existential.id) match {
      case Some(a) =>
        assertTypeMatch(errors, loc)(a, t)
      case None =>
        assignGeneric(existential.id, t)
    }
  }

  private def instantiateR(errors: ListBuffer[Diagnostic], loc: Loc)
    (a: Type, existential: ExistentialInstance) = {
    getGeneric(existential.id) match {
      case Some(b) =>
        assertTypeMatch(errors, loc)(a, b)
      case None =>
        assignGeneric(existential.id, a)
    }
  }

  def getGeneric(i: Int): Option[Type] = {
    _genericInstances.get(i)
  }

  def assignGeneric(i: Int, value: Type): Unit = {
    _genericInstances.update(i, value)
  }


  def checkPattern(typ: Type)(pattern: P.Pattern): T.Pattern = {
    import P.{Pattern => PP}
    import T.{Pattern => TP}
    pattern match {
      case PP.Var(meta, ident) =>
        val typedIdent = inferVarBindingIdent(ident, typ)
        setTypeOfSymbol(typedIdent.name, typ)
        TP.Var(meta.typed(typedIdent.meta.typ), typedIdent)
      case PP.Paren(meta, inner) =>
        val checkedInner = checkPattern(typ)(inner)
        TP.Paren(meta.typed(checkedInner.meta.typ), checkedInner)
      case PP.Annotated(meta, inner, typeAnnotation) =>
        val errors = ListBuffer.empty[Diagnostic]
        val inferredAnnotation = inferTypeAnnotation(typeAnnotation)
        val checkedInner = checkPattern(inferredAnnotation.meta.typ)(inner)
        assertTypeMatch(
          errors, inferredAnnotation.loc
        )(expected = typ, found = inferredAnnotation.meta.typ)
        TP.Annotated(
          meta.typed(typ).withDiagnostics(errors),
          checkedInner,
          inferredAnnotation
        )
    }
  }

  def setTypeOfSymbol(symbol: Symbol, typ: Type): Unit = {
    _symbolType.update(symbol, typ)
  }

  def getTypeOfSymbol(symbol: Symbol): Option[Type] = {
    _symbolType.get(symbol)
  }

  def inferTypeAnnotation(annotation: P.TypeAnnotation): T.TypeAnnotation = {
    annotation match {
      case P.TypeAnnotation.Var(meta, ident) =>
        val inferredIdent = inferTypeVarIdent(ident)
        T.TypeAnnotation.Var(
          meta.typed(inferredIdent.meta.typ),
          inferredIdent
        )
      case forall: P.TypeAnnotation.Forall =>
        val inferredParams = inferGenericParams(forall.genericParams)
        val inferredBody = inferTypeAnnotation(forall.inner)
        val typ = inferredParams
          .foldRight(inferredBody.meta.typ)((param, body) =>
            Forall(param.ident.name, body)
          )
        T.TypeAnnotation.Forall(
          forall.meta.typed(typ),
          forall.forallScope,
          inferredParams,
          inferredBody
        )
      case func: P.TypeAnnotation.Func =>
        val from = func.params.map({
          case (Some(label), t) =>
            val annotationType = inferTypeAnnotation(t)
            val ident = inferVarBindingIdent(label, annotationType.meta.typ)
            Some(ident) -> annotationType
          case (None, t) =>
            None -> inferTypeAnnotation(t)
        })
        val to = inferTypeAnnotation(func.returnType)
        val typ = Func(
          from.map({
            case (label, t) =>
              (label.map(_.name), t.meta.typ)
          }),
          to.meta.typ
        )
        T.TypeAnnotation.Func(
          func.meta.typed(typ),
          func.funcScope,
          from,
          to
        )
      case t: P.TypeAnnotation.TApplication =>
        val func = inferTypeAnnotation(t.tFunc)
        val args = checkTypeApplication(getKindOfType(func.meta.typ), t.args)
        val typ = args.foldLeft(func.meta.typ)((f, arg) =>
          TApplication(f, arg.meta.typ))
        T.TypeAnnotation.TApplication(
          t.meta.typed(typ),
          func, args
        )
      case p: P.TypeAnnotation.Prop =>
        inferPropAnnotation(p)
      case P.TypeAnnotation.Paren(meta, inner) =>
        val inferredInner = inferTypeAnnotation(inner)
        T.TypeAnnotation.Paren(
          meta.typed(inferredInner.meta.typ),
          inferredInner
        )

    }
  }

  def checkTypeApplication(
    fKind: Kind,
    annotations: Array[P.TypeAnnotation]
  ): Array[T.TypeAnnotation] = {
    if (annotations.length == 0) {
      Array.empty
    } else {
      val head = inferTypeAnnotation(annotations(0))
      val tail = annotations.tail
      val headKind = getKindOfType(head.meta.typ)
      (fKind, headKind) match {
        case (Kind.KFun(from, to), _) if headKind == from =>
          head +: checkTypeApplication(to, tail)
        case (Kind.KFun(from, to), _) =>
          head.withMeta(
            head.meta.withDiagnostic(
              Diagnostic(
                loc = head.loc,
                severity = Severity.Error,
                variant = KindMismatch(
                  from,
                  headKind
                )
              )
            )
          ) +: checkTypeApplication(to, tail)

      }
    }
  }

  def getKindOfType(t: Type): Kind = {
    t match {
      case Var(v) => getKindOfSymbol(v).get
      case Constructor(_, kind) => kind
      case Forall(_, inner) => getKindOfType(inner)
      case Func(_, _) => Kind.Star
      case TApplication(f, arg) =>
        getKindOfType(f) match {
          case Kind.KFun(_, to) => to
          case Kind.Star => Kind.Star
        }
    }
  }

  def inferGenericParams(params: Array[P.GenericParam]): Array[T.GenericParam] = {
    params.map(inferGenericParam)
  }

  def inferGenericParam(param: P.GenericParam): T.GenericParam = {
    val ident = inferTypeVarBindingIdent(param.ident)
    val kindAnnotation = param.kindAnnotation.map(inferKindAnnotation)
    val kind = kindAnnotation.map(getKindFromAnnotation).getOrElse(Kind.Star)
    setKindOfSymbol(ident.name, kind)
    T.GenericParam(
      param.meta.typed(Var(ident.name)),
      ident,
      kindAnnotation
    )
  }

  def getTypeVar(symbol: Symbol): Option[Type] = {
    _typeVars.get(symbol)
  }

  def setTypeVar(symbol: Symbol, typ: Type) = {
    _typeVars.update(symbol, typ)
  }

  def inferTypeVarIdent(ident: P.Ident): T.Ident = {
    val name = ident.name
    getTypeDeclOf(ident.name, ident) match {
      case Some(decl) =>
        Debug.log(s"$name: found declaration")
        inferDeclaration(decl)
      case None => ()
    }
    val result = ident.scope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(symbol)) =>
        Debug.log(s"$name type entry found in scope")
        T.Ident(
          meta = ident.meta.typed(getTypeVar(symbol).getOrElse(Var(symbol))),
          name = symbol
        )
      case None =>
        Debug.log(s"$name: type entry not found")
        val symbol = ctx.makeSymbol(ident.name)
        T.Ident(
          meta = ident.meta.typed(Var(symbol)).withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = UnBoundTypeVar(ident.name)
            )
          ),
          symbol
        )
    }
    result
  }

  def inferVarIdent(ident: P.Ident): T.Ident = {
    val errors = ListBuffer.empty[Diagnostic]
    val (typ, symbol) = ident.scope.resolveEntry(ident.name) match {
      case Some(ScopeEntry(symbol, _)) =>
        (getTypeOfSymbol(symbol).getOrElse(Uninferred), symbol)
      case None =>
        getDeclOfVar(ident.name, ident) match {
          case Some(d: P.Declaration.Let) =>
            if (d.loc.start >= ident.loc.end) {
              errors.append(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UseBeforeAssignment(ident.name)
                )
              )
            } else {
              errors.append(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UnBoundVar(ident.name)
                )
              )
            }
          case _ => ()
        }

        (Uninferred, ctx.makeSymbol(ident.name))
    }
    T.Ident(
      ident.meta.typed(typ).withDiagnostics(errors),
      symbol
    )
  }

  def inferTypeVarBindingIdent(ident: P.Ident, typOpt: Option[Type] = None): T.Ident = {
    ident.scope.typeSymbols.get(ident.name) match {
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        ident.scope.setTypeVar(ident.name, TypeEntry(symbol))

        val typ = typOpt match {
          case Some(t) =>
            setTypeVar(symbol, t)
            t
          case None =>
            Uninferred
        }
        T.Ident(
          meta = ident.meta.typed(typ),
          name = symbol
        )
      case Some(TypeEntry(symbol)) =>
        T.Ident(
          meta = ident.meta.typed(Primitive.Unit).withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = DuplicateBinding(symbol.text)
            )
          ),
          name = ctx.makeSymbol(symbol.text)
        )
    }
  }

  def inferVarBindingIdent(ident: P.Ident, typ: Type): T.Ident = {
    ident.scope.getSymbol(ident.name) match {
      case Some(symbol) =>
        T.Ident(
          meta = ident.meta.typed(typ).withDiagnostic(
            Diagnostic(
              loc = ident.loc,
              severity = Severity.Error,
              variant = DuplicateBinding(ident.name)
            )
          ),
          name = symbol
        )
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        ident.scope.setSymbol(ident.name, ScopeEntry(symbol))
        setTypeOfSymbol(symbol, typ)
        T.Ident(
          meta = ident.meta.typed(typ),
          name = symbol
        )
    }
  }

  def getDeclOfNode(node: P.Node): Option[P.Declaration] = for {
    parentId <- node.meta.parentId
    node <- ctx.getParsedNode(parentId)
    decl <- node match {
      case d: P.Declaration => Some(d)
      case _ => getDeclOfNode(node)
    }
  } yield decl

  def getTypeDeclOf(name: String, node: P.Node): Option[P.Declaration] = {
    node.meta.parentId match {
      case Some(parentId) =>
        ctx.getParsedNode(parentId) match {
          case Some(P.SourceFile(meta, _, declarations)) =>
            declarations.find(isTypeDeclarationOf(name))
          case Some(n) =>
            getTypeDeclOf(name, n)
          case None => None
        }
      case None => None
    }
  }

  def getDeclOfVar(name: P.Name, node: P.Node): Option[P.Declaration] = {
    node.meta.parentId match {
      case Some(parentId) =>
        ctx.getParsedNode(parentId) match {
          case Some(P.SourceFile(_, _, declarations)) =>
            declarations.find(isVarDeclOf(name))
          case Some(n) =>
            getDeclOfVar(name, n)
          case None =>
            None
        }
      case None => None
    }
  }

  def isVarDeclOf(name: String)(decl: P.Declaration): Boolean = {
    decl match {
      case d: P.Declaration.Let =>
        doesPatternBind(name, d.pattern)
      case _ => false
    }
  }

  def doesPatternBind(name: String, pattern: P.Pattern): Boolean = {
    pattern match {
      case P.Pattern.Var(_, ident) if ident.name == name => true
      case _: P.Pattern.Var =>  false
      case P.Pattern.Annotated(_, inner, _) => doesPatternBind(name, inner)
      case P.Pattern.Function(_, f, params) =>
        doesPatternBind(name, f) || params.exists({
          case P.Pattern.Param.SubPattern(_, _, p) => doesPatternBind(name, p)
          case P.Pattern.Param.Rest(_) =>  false
        })
      case P.Pattern.DotName(_, _) => false
    }
  }

  def isTypeDeclarationOf(typeName: String)(decl: P.Declaration): Boolean = decl match {
    case alias: P.Declaration.TypeAlias if alias.ident.name == typeName => true
    case _: P.Declaration.TypeAlias => false
    case _: P.Declaration.Let => false
  }


  private def checkModifiers(
    errors: ListBuffer[diagnostics.Diagnostic]
  )(modifiers: Iterable[P.Declaration.Modifier]) = {

  }
}
