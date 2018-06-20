package lmc

import lmc.diagnostics._
import lmc.types._

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
        hd match {
          case h: HasDeclaration =>
            solveConstraints(hasDeclaration(h, tl))
          case u: Unifies =>
            solveConstraints(unifies(u, tl))
          case h: HasProperty =>
            solveConstraints(hasProperty(h, tl))
          case h: HasKind =>
            solveConstraints(hasKind(h, tl))
        }
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
