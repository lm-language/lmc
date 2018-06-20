package lmc

import lmc.diagnostics._
import lmc.types._

import scala.collection.mutable.ListBuffer

class ConstraintSolver(
  checker: TypeChecker,
  error: Diagnostic => Unit,
) {
  var i = 0
  val MAX_LOOPS = 500

  def solveConstraints(
    constraints: List[Constraint]
  ): Unit = {
    i += 1
    if (i > MAX_LOOPS) {
      error(
        Diagnostic(
          loc = constraints.head.loc,
          severity = Severity.Error,
          variant = ConstraintLoopOverflow
        )
      )
      return
    }
    constraints match {
      case Nil =>
        for ((k, v) <- checker.symbolTypes) {
          checker.symbolTypes.update(k, checker.applyEnv(v))
        }
      case hd::tl =>
        solveConstraints(hd match {
          case h: HasDeclaration =>
            hasDeclaration(h, tl)
          case u: Unifies =>
            unifies(u, tl)
          case h: HasProperty =>
            hasProperty(h, tl)
          case h: HasKind =>
            hasKind(h, tl)
          case h: HasReturnType =>
            hasReturnType(h, tl)
          case t: TakesPositionalArg =>
            takesPositionalParam(t, tl)
          case t: TakesLabeledArg =>
            takesLabeledArg(t, tl)
          case f: FunctionApplication =>
            functionApplication(f, tl)
          case f: IsFunction =>
            isFunction(f, tl)
        })
    }
  }

  private def functionApplication(
    f: FunctionApplication,
    constraints: List[Constraint]
  ): List[Constraint] = {
    val missingArguments = ListBuffer.empty[(Option[(lmc.common.Loc, String)], Type)]
    f.func match {
      case Func(params, _) =>
        val expectedLabeledArgs = params.filter({ _._1.isDefined }).map(arg => {
          (arg._1.get.text, arg._2)
        }).toMap
        val labelChecked = collection.mutable.Set.empty[String]
        var i = 0
        val missingArguments = ListBuffer.empty[(Option[String], Type)]
        var labelEncountered = false
        val labeledArgsToCheck = ListBuffer.empty[((String, lmc.common.Loc), Type)]
        for (expectedArg <- params) {
          if (i >= f.args.length) {
            missingArguments.append(
              expectedArg._1.map(_.text) -> expectedArg._2
            )
            expectedArg._1 match {
              case Some(s) => labelChecked += s.text
              case None => ()
            }
          } else {
            val arg = f.args(i)
            arg._2 match {
              case Some(label) =>
                labelEncountered = true
                labeledArgsToCheck.append(label -> arg._3)
              case None =>
                if (labelEncountered) {
                  error(Diagnostic(
                    loc = arg._1,
                    severity = Severity.Error,
                    variant = PositionalArgAfterLabelled
                  ))
                } else {
                }
                expectedArg._1 match {
                  case None => ()
                  case Some(label) =>
                    labelChecked += label.text
                }
            }
          }
          i += 1
        }
        while (i < f.args.length) {
          val arg = f.args(i)
          arg._2 match {
            case Some(label) =>
              labelEncountered = true
              labeledArgsToCheck.append(label -> arg._3)
            case None =>
              error(
                Diagnostic(
                  loc = arg._1,
                  severity = Severity.Error,
                  variant = ExtraArg
                )
              )
          }
          i += 1
        }
        for (((name, loc), expr) <- labeledArgsToCheck) {
          expectedLabeledArgs.get(name) match {
            case Some(expectedTyp) =>
              val diagnostic = if (labelChecked.contains(name)) {
                error(Diagnostic(
                  loc = loc,
                  severity = Severity.Error,
                  variant = DuplicateLabelArg(name)
                ))
              } else {
                None
              }
              labelChecked += name
            case None =>
              error(
                Diagnostic(
                  loc = loc,
                  severity = Severity.Error,
                  variant = NoSuchParamLabel(name)
                )
              )
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
          error(
            Diagnostic(
              loc = f.loc,
              severity = Severity.Error,
              variant = MissingArguments(missingArguments)
            )
          )
        }
        constraints
      case Var(_) | ExistentialInstance(_, _) =>
        constraints :+ f
      case typ =>
        constraints
    }
  }

  private def isFunction(
    f: IsFunction,
    constraints: List[Constraint]
  ): List[Constraint] = {
    f.typ match {
      case Func(_, _) => constraints
      case Var(_) | ExistentialInstance(_, _) =>
        constraints :+ f
      case typ =>
        error(
          Diagnostic(
            loc = f.loc,
            severity = Severity.Error,
            variant = NotAFunction(typ)
          )
        )
        constraints
    }
  }

  private def takesPositionalParam(
    t: TakesPositionalArg, constraints: List[Constraint]
  ): List[Constraint] = {
    t.t match {
      case Func(from, to) =>
        if (from.length <= t.index) {
          constraints
        } else {
          val positionalParam = from(t.index)
          Unifies(t.loc, positionalParam._2, t.argType)::constraints
        }
      case _: ExistentialInstance | _: Var =>
        constraints ++ List(t)
      case _ =>
        constraints
    }
  }

  private def takesLabeledArg(
    t: TakesLabeledArg,
    constraints: List[Constraint]
  ): List[Constraint] = {
    t.t match {
      case Func(from, to) =>
        val positionalParam = from.find(
          p => p._1.map(_.text).contains(t.label._2)
        )
        positionalParam match {
          case Some((_, paramTyp)) =>
            Unifies(t.loc, paramTyp, t.paramTyp)::constraints
          case None =>
            constraints
        }
      case _: ExistentialInstance | _: Var =>
        constraints ++ List(t)
      case _ =>
        constraints
    }
  }

  private def hasReturnType(
    h: HasReturnType,
    constraints: List[Constraint]
  ): List[Constraint] = {
    h.t match {
      case Func(from, to) =>
        Unifies(h.loc, to, h.returnType)::constraints
      case ExistentialInstance(_, _) | Var(_) =>
        constraints ++ List(h)
      case _ =>
        constraints

    }
  }

  private def hasKind(
    h: HasKind, constraints: List[Constraint]
  ): List[Constraint] = {
    val kind = getKindOfType(h.t)
    if (kind != h.kind) {
      error(
        Diagnostic(
          loc = h.loc,
          severity = Severity.Error,
          variant = KindMismatch(h.kind, kind)
        )
      )
    }
    constraints
  }

  private def getKindOfType(t: Type): Kind = {
    t match {
      case Var(name) =>
        checker.getKindOfSymbol(name).getOrElse(Kind.Star)
      case Constructor(_, kind) =>
        kind
      case Func(_, _) => Kind.Star
      case Forall(_, to) => getKindOfType(to)
    }
  }

  private def unifies(
    u: Unifies, constraints: List[Constraint]
  ): List[Constraint] = {
    (u.expected, u.found) match {
      case (Constructor(s1, _), Constructor(s2, _)) if s1.id == s2.id =>
        constraints
      case (Var(s1), Var(s2)) if s1.id == s2.id =>
        constraints
      case (t, Var(s1)) if checker.getTypeVar(s1).isDefined =>
         checker.getTypeVar(s1) match {
          case Some(t1) =>
            Unifies(u.loc, t, t1)::constraints
          case None =>
            constraints
        }
      case (Var(s1), t) if checker.getTypeVar(s1).isDefined =>
         checker.getTypeVar(s1) match {
          case Some(t1) =>
            Unifies(u.loc, t1, t)::constraints
          case None =>
            constraints
        }
      case (ExistentialInstance(id, _), t) =>
        checker.generics.update(id, t)
        constraints.map(applyEnv)
      case (t, ExistentialInstance(id, _)) =>
        checker.generics.update(id, t)
        constraints.map(applyEnv)
      case (Func(from1, to1), Func(from2, to2)) =>
        // TODO: unify params as well
        Unifies(u.loc, to1, to2)::constraints
      case _ =>
        error(Diagnostic(
          loc = u.loc,
          severity = Severity.Error,
          variant = TypeMismatch(u.expected, u.found)
        ))
        constraints
    }
  }


  private def applyEnv(constraint: Constraint): Constraint = {
    constraint match {
      case HasProperty(loc, t1, prop, t2) =>
        HasProperty(loc, checker.applyEnv(t1), prop, checker.applyEnv(t2))
      case HasDeclaration(loc, t1, prop, t2) =>
        HasDeclaration(loc, checker.applyEnv(t1), prop, checker.applyEnv(t2))
      case Unifies(loc, t1, t2) =>
        Unifies(loc, checker.applyEnv(t1), checker.applyEnv(t2))
      case TakesLabeledArg(loc, t1, name, typ) =>
        TakesLabeledArg(
          loc,
          checker.applyEnv(t1),
          name,
          checker.applyEnv(typ)
        )
      case TakesPositionalArg(loc, t1, index, typ) =>
        TakesPositionalArg(
          loc,
          checker.applyEnv(t1),
          index,
          checker.applyEnv(typ)
        )
      case HasReturnType(loc, t1, returnType) =>
        HasReturnType(
          loc,
          checker.applyEnv(t1),
          checker.applyEnv(returnType)
        )
      case IsFunction(loc, typ) =>
        IsFunction(
          loc,
          checker.applyEnv(typ)
        )
      case FunctionApplication(loc, f, args) =>
        FunctionApplication(
          loc,
          checker.applyEnv(f),
          args.map({
            case (l, label, t) => (l, label, checker.applyEnv(t))
          })
        )
    }
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
}
