import Syntax._

import scala.collection.mutable
import Diagnostics._


final class Typechecker(compiler: Compiler, setTypeOfSymbol: (Symbol, Type) => Unit) {
  import Syntax.{ Parsed => P }
  import Syntax.{ Typed => T }

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
      for (decl <- declarations) {

      }
      declarations
        .map(preAddScopeDeclarations)
        .map(nameDeclaration)
        .map(checkDeclaration)
    })

  private def addPreNameSymbolsToScope(binder: P.Binder): P.Binder = {
    binder.pattern.variant match {
      case P.Pattern.Var(ident) =>
        val diagnostics = addBindingToEnclosingScope(
          loc = binder.loc, symbol = compiler.makeSymbol(ident.name),
          typ = UnAssigned()
        )
        val pattern = binder.pattern.copy(
          meta = binder.pattern.meta.copy(
            diagnostics = binder.meta.diagnostics ++ diagnostics
          )
        )
        binder.copy(
          pattern = pattern
        )
      case P.Pattern.Error() => binder
    }
  }

  private def preAddScopeDeclarations(decl: P.Declaration): P.Declaration = {
    decl.variant match {
      case P.Declaration.Let(binder, rhs) =>
        val newBinder = addPreNameSymbolsToScope(binder)
        decl.copy(
          variant = P.Declaration.Let(newBinder, rhs))
      case P.Declaration.Error() =>
        decl
    }
  }

  private def nameDeclaration(declaration: Parsed.Declaration): Typed.Declaration = {
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

  var nextUnInferredId = 0

  private def nameBindingIdent(ident: Parsed.Ident): Typed.Ident = {
    val scope = enclosingScope
    scope.symbols.get(ident.name) match {
      case Some(ScopeEntry(symbol, _, _)) =>
        Typed.Ident(meta = ident.meta, name = symbol)
      case None =>
        throw new Error("CompilerBug: Binding should've been added by pre-naming phase")
    }
  }

  /**
    * Add a declaration binding to scope, returning errors in case
    * binding is already added.
    * @param loc
    * @param symbol
    * @param typ
    * @return
    */
  private def addBindingToEnclosingScope(loc: Loc, symbol: Symbol, typ: Type): Iterable[Diagnostic] = {
    val scope = enclosingScope
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
        setTypeOfSymbol(symbol, typ)
        List()
    }
  }

  private def enclosingScope: MutableScope =
    scopeMaps.head


  private def nameExpr(expr: Parsed.Expr): Typed.Expr = {
    val variant = expr.variant match {
      case P.Expr.Literal(P.Expr.LInt(v)) =>
        T.Expr.Literal(T.Expr.LInt(v))
      case P.Expr.Var(ident) =>
        val namedIdent = nameVarIdent(ident)
        T.Expr.Var(namedIdent)
      case P.Expr.Error() => T.Expr.Error()
    }
    T.Expr(meta = expr.meta, variant)
  }

  private def nameVarIdent(ident: P.Ident): T.Ident = {
    println("namingVarIdent")
    println(this.scopeMaps)
    println(ident.name, resolveName(ident.name))
    resolveName(ident.name) match {
      case Some(ScopeEntry(symbol, UnAssigned(), _)) =>
        T.Ident(ident.meta.copy(
          diagnostics = ident.meta.diagnostics ++ List(
            Diagnostic(
              loc = ident.meta.loc,
              variant = Diagnostics.UseBeforeAssignment(ident.name),
              severity = Severity.Error
            )
          )
        ), symbol)
      case None =>
        val symbol = compiler.makeSymbol(ident.name)
        T.Ident(ident.meta.copy(
          diagnostics = ident.meta.diagnostics ++ List(
            Diagnostic(
              loc = ident.meta.loc,
              variant = Diagnostics.UnBoundVar(ident.name),
              severity = Severity.Error
            )
          )
        ), symbol)
      case Some(entry) =>
        T.Ident(ident.meta, entry.symbol)
    }
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
      case Var(ident) =>
        val (typ, diagnostics) = getType(ident.loc, ident.name)
        (expr.variant, typ, diagnostics)
      case Error() => (expr.variant, ErrorType(), List())
    }
    (expr.copy(
      meta = expr.meta.copy(
        diagnostics = expr.meta.diagnostics ++ diagnostics
      ),
      variant = variant
    ), typ)
  }

  def getType(loc: Loc, symbol: Symbol): (Type, Iterable[Diagnostic]) = {
    compiler.getType(symbol) match {
      case None =>
        (ErrorType(), List(
          Diagnostic(
            variant = UnBoundVar(symbol.text),
            severity = Severity.Error,
            loc = loc)))
      case Some(t) =>
        (t, List())
    }
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
    println("Setting type of binding ", ident.name)
    setTypeOfSymbol(ident.name, typ)
    ident
  }

  private def setBindingInEnclosingScope(loc: Loc, symbol: Symbol, typ: Type): Iterable[Diagnostic] = {
    val scope = enclosingScope
    val entry = ScopeEntry(symbol = symbol, typ = typ, loc = loc)
    scope.symbols += symbol.text -> entry
    List()
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

  private def resolveName(name: String): Option[ScopeEntry] = {
    var l = this.scopeMaps
    while (true) {
      l match {
        case hd::tl =>
          hd.symbols.get(name) match {
            case Some(entry) => return Some(entry)
            case None =>
              l = tl
          }
        case _ =>
          return None
      }
    }
    None
  }

  private def withNewScope[T](loc: Loc)(f: () => T): (T, Scope) = {
    pushScope(loc)
    val result = f()
    val scope = popScope()
    (result, scope)
  }
}