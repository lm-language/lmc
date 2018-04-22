import Syntax._

import scala.collection.mutable
import Diagnostics._

object Typechecker {
  def apply(compiler: Compiler): Typechecker = new Typechecker(compiler)
}

final class Typechecker(compiler: Compiler) {
  import Syntax.{ Parsed => P }
  import Syntax.{ Typed => T }
  case class MutableScope(
    var loc: Loc,
    var symbols: mutable.HashMap[String, ScopeEntry] = mutable.HashMap.empty,
    children: mutable.ArrayBuffer[Scope] = mutable.ArrayBuffer.empty,
  ) {
    def toScope: Scope = {
      Scope(
        loc = loc,
        symbols = symbols.toMap,
        children = this.children
      )
    }
  }
  private var scopeMaps = List.empty[MutableScope]

  def checkSourceFile(parsed: Parsed.SourceFile): Typed.SourceFile = {
    val (declarations, scope) = checkModule(parsed.loc, parsed.declarations)
    Typed.SourceFile(
      parsed.meta,
      declarations = declarations,
      scope = scope
    )
  }

  /**
    * Type checking a module involves the following steps
    * 1. Push a new scope for the module
    * 2. Walk over all declarations, adding all bindings
    *    declared in the module, with the type UnBound
    * 3. Walk over declarations again, this time inferring
    *    the actual types of bindings, assigning UnInferred
    *    for declarations that can't be inferred yet. As we
    *    go along, we keep adding type constraints.
    * 4. Solve all type constraints generated. Ensure that
    *    no un-inferred type remains after solving the constraints.
    *    If there are un-inferred types, add errors to their corresponding
    *    declaration nodes.
    * @param loc Loc of module/source file
    * @param declarations List of declarations
    * @return
    */
  private def checkModule(loc: Loc, declarations: Iterable[Parsed.Declaration]): (Iterable[Typed.Declaration], Scope) =
    withNewScope(loc)(() => {
      declarations
        .map(nameDeclarations)
        .map(checkDeclaration)
    })

  private def nameDeclarations(declaration: Parsed.Declaration): Typed.Declaration = {
    val variant = declaration.variant match {
      case P.Declaration.Let(binder, expr) =>
        T.Declaration.Let(nameBinder(binder), nameExpr(expr))
      case P.Declaration.Error() =>
        T.Declaration.Error()
    }
    T.Declaration(declaration.meta, variant)
  }

  private def nameBinder(binder: Parsed.Binder): Typed.Binder = {
    Typed.Binder(meta = binder.meta, pattern = namePattern(binder.pattern))
  }

  private def namePattern(pattern: Syntax.Parsed.Pattern): Typed.Pattern = {
    val variant = pattern.variant match {
      case P.Pattern.Var(ident) =>
        val namedIdent = nameBindingIdent(ident)
        T.Pattern.Var(namedIdent)
      case P.Pattern.Error() =>
        T.Pattern.Error()
    }
    T.Pattern(meta = pattern.meta, variant = variant)
  }

  private def nameBindingIdent(ident: Parsed.Ident): Typed.Ident = {
    val symbol = compiler.makeSymbol(ident.name)
    val diagnostics = addBindingToEnclosingScope(ident.meta.loc, symbol, UnBound())
    Typed.Ident(meta = ident.meta.copy(diagnostics = diagnostics), name = symbol)
  }

  private def addBindingToEnclosingScope(loc: Loc, symbol: Symbol, typ: Type): Iterable[Diagnostic] = {
    scopeMaps match {
      case scope::_ =>
        scope.symbols.get(symbol.text) match {
          case Some(_) =>
            List(Diagnostic(
              loc = loc,
              severity = Severity.Error,
              variant = DuplicateBinding(symbol.text)
            ))
          case None =>
            val entry = ScopeEntry(symbol = symbol, typ = typ, loc = loc)
            scope.symbols += symbol.text -> entry
            List()
        }
      case Nil => throw new Error("CompilerBug: Can't add type to empty scope list")
    }
  }


  private def nameExpr(expr: Parsed.Expr): Typed.Expr = {
    val variant = expr.variant match {
      case P.Expr.Literal(P.Expr.LInt(v)) =>
        T.Expr.Literal(T.Expr.LInt(v))
      case P.Expr.Error() => T.Expr.Error()
    }
    T.Expr(meta = expr.meta, variant)
  }

  private def checkDeclaration(declaration: Typed.Declaration): Typed.Declaration = {
    import Syntax.Typed._
    val variant = declaration.variant match {
      case Declaration.Let(binder, expr) =>
        val (inferredExpr, typ) = inferExpr(expr)
        val boundBinder = bindTypeToBinder(binder, typ)
        Declaration.Let(binder = boundBinder, rhs = inferredExpr)
      case Declaration.Error() =>
        declaration.variant
    }
    Declaration(meta = declaration.meta, variant = variant)
  }

  private def inferExpr(expr: Typed.Expr): (Typed.Expr, Type) = {
    import Typed._
    import Expr._
    val (variant, typ, diagnostics) = expr.variant match {
      case Literal(LInt(_)) =>
        (expr.variant, compiler.IntType, List())
      case Error() => (expr.variant, UnBound(), List())
    }
    (expr.copy(
      meta = expr.meta.copy(
        diagnostics = expr.meta.diagnostics ++ diagnostics
      ),
      variant = variant
    ), typ)
  }

  private def bindTypeToBinder(binder: Typed.Binder, typ: Type): Typed.Binder = {
    binder.copy(pattern = bindTypeToPattern(binder.pattern, typ))
  }

  private def bindTypeToPattern(pattern: Typed.Pattern, typ: Type): Typed.Pattern = {
    import Typed._
    import Pattern._
    val (variant, diagnostics) = pattern.variant match {
      case Var(ident) =>
        val newIdent = bindTypeToIdent(ident, typ)
        (Var(newIdent), List())
      case Error() =>
        (pattern.variant, List())
    }
    Pattern(meta = pattern.meta.copy(
      diagnostics = pattern.meta.diagnostics ++ diagnostics
    ), variant = variant)
  }

  private def bindTypeToIdent(ident: Syntax.Typed.Ident, typ: Type): Typed.Ident = {
    setBindingInEnclosingScope(ident.loc, ident.name, typ)
    ident
  }

  private def setBindingInEnclosingScope(loc: Loc, symbol: Symbol, typ: Type): Iterable[Diagnostic] = {
    scopeMaps match {
      case scope::_ =>
        val entry = ScopeEntry(symbol = symbol, typ = typ, loc = loc)
        scope.symbols += symbol.text -> entry
        List()
      case Nil => throw new Error("CompilerBug: Can't add type to empty scope list")
    }
  }

  private def pushScope(loc: Loc): Unit = {
    scopeMaps = MutableScope(loc)::scopeMaps
  }

  private def popScope(): Scope = {
    scopeMaps match {
      case Nil => throw new Exception("CompilerBug: Tried to pop scope from empty list")
      case mutableScope::tail =>
        scopeMaps = tail
        val scope = mutableScope.toScope
        tail match {
          case parent::_ =>
            parent.children += scope
          case  _ => ()
        }
        scope

    }
  }

  private def withNewScope[T](loc: Loc)(f: () => T): (T, Scope) = {
    pushScope(loc)
    val result = f()
    val scope = popScope()
    (result, scope)
  }
}