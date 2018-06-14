package lmc

import lmc.common.{Loc, ScopeEntry, Symbol, TypeEntry}
import lmc.diagnostics.{Diagnostic, Severity, TypeMismatch, UnBoundTypeVar}
import lmc.types._
import lmc.utils.Debug

import scala.collection.mutable.ListBuffer


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

  def inferSourceFile(sourceFile: P.SourceFile): T.SourceFile = {
    val inferredDeclarations = sourceFile.declarations.map(inferDeclaration)
    T.SourceFile(
      sourceFile.meta.typed(getModuleTypeFromDeclarations(inferredDeclarations)),
      sourceFile.scope,
      inferredDeclarations
    )
  }

  def getModuleTypeFromDeclarations(declarations: Array[T.Declaration]): Type = {
    // TODO
    Uninferred
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
      case PD.Let(meta, modifiers, P.Pattern.Annotated(pMeta, innerPattern, annotation), Some(rhs)) =>
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
      case PD.TypeAlias(meta, modifiers, ident, kindAnnotation, rhs) =>
        checkModifiers(errors)(modifiers)
        val checkedKindAnnotation = kindAnnotation.map(inferKindAnnotation)
        val kind = checkedKindAnnotation match {
          case Some(kindAnnotation) =>
            getKindFromAnnotation(kindAnnotation)
          case None =>
            Kind.Star
        }
        val typedRhs = rhs.map(inferTypeAnnotation)
        val typ = typedRhs match {
          case Some(r) =>
            r.meta.typ
          case None => Uninferred
        }
        val inferredIdent = inferTypeVarBindingIdent(ident, typ)
        setKindOfSymbol(inferredIdent.name, kind)
        decl.scope.addDeclaration(inferredIdent.name, meta.id)
        TD.TypeAlias(
          meta.typed(Primitive.Unit),
          modifiers.map(_.typed),
          inferredIdent,
          checkedKindAnnotation,
          typedRhs
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
    ???
  }

  def inferKindAnnotation(annot: P.KindAnnotation): T.KindAnnotation = {
    ???
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
    }
  }

  def inferPattern(pattern: P.Pattern): T.Pattern = {
    ???
  }

  def checkExpr(typ: Type)(expr: P.Expression): T.Expression = {
    expr match {
      case _ =>
        val inferredExpr = inferExpr(expr)
        val errors = ListBuffer.empty[Diagnostic]
        assertSubType(
          errors, inferredExpr.loc
        )(expected = typ, found = inferredExpr.meta.typ)
        exprWithMeta(inferredExpr, inferredExpr.meta.withDiagnostics(
          errors
        ))
    }
  }

  def exprWithMeta(e: T.Expression, meta: T.Meta): T.Expression = {
    e match {
      case v: T.Expression.Var => v.copy(meta)
      case e: T.Expression.Error => e.copy(meta)
      case l: T.Expression.Literal => l.copy(meta)
      case c: T.Expression.Call => c.copy(meta)
      case f: T.Expression.Func => f.copy(meta)
      case w: T.Expression.With => w.copy(meta)
      case b: T.Expression.Block => b.copy(meta)
    }
  }

  def assertSubType(
    errors: ListBuffer[diagnostics.Diagnostic],
    loc: Loc
  )(
    expected: Type,
    found: Type
  ) = {
    if (expected != found) {
      errors.append(
        Diagnostic(
          loc = loc,
          severity = Severity.Error,
          variant = TypeMismatch(
            expected,
            found
          )
        )
      )
    }
  }

  def checkPattern(typ: Type)(pattern: P.Pattern): T.Pattern = {
    import P.{Pattern => PP}
    import T.{Pattern => TP}
    pattern match {
      case PP.Var(meta, ident) =>
        val typedIdent = inferVarBindingIdent(ident, typ)
        setTypeOfSymbol(typedIdent.name, typ)
        TP.Var(meta.typed(typedIdent.meta.typ), typedIdent)
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
        println(s"${inferredIdent.name}: ${inferredIdent.meta.typ}")
        T.TypeAnnotation.Var(
          meta.typed(inferredIdent.meta.typ),
          inferredIdent
        )

    }
  }

  def getTypeVar(symbol: Symbol): Option[Type] = {
    _typeVars.get(symbol)
  }

  def setTypeVar(symbol: Symbol, typ: Type) = {
    _typeVars.update(symbol, typ)
  }

  def inferTypeVarIdent(ident: P.Ident): T.Ident = {
    if (ident.name == "I") {
      Debug.start()
    }
    val name = ident.name
    Debug.log(s"$name inferTypeVarIdent(${ident.name})")
    ident.scope.resolveTypeEntry(ident.name) match {
      case Some(TypeEntry(symbol)) =>
        Debug.log(s"$name type entry found in scope")
        T.Ident(
          meta = ident.meta.typed(getTypeVar(symbol).getOrElse(Uninferred)),
          name = symbol
        )
      case None =>
        Debug.log(s"$name: type entry not found in scope")
        getTypeDeclOf(ident.name, ident) match {
          case Some(decl) =>
            Debug.log(s"$name: found declaration")
            inferDeclaration(decl)
          case None => ()
        }
        ident.scope.resolveTypeEntry(ident.name) match {
          case Some(TypeEntry(symbol)) =>
            Debug.log(s"$name: type entry found")
            val typ = getTypeVar(symbol).getOrElse(Uninferred)
            T.Ident(
              meta = ident.meta.typed(
                typ
              ),
              symbol
            )
          case None =>
            Debug.log(s"$name: type entry not found")
            val symbol = ctx.makeSymbol(ident.name)
            T.Ident(
              meta = ident.meta.typed(Uninferred).withDiagnostic(
                Diagnostic(
                  loc = ident.loc,
                  severity = Severity.Error,
                  variant = UnBoundTypeVar(ident.name)
                )
              ),
              symbol
            )
        }
    }
  }

  def inferVarIdent(ident: P.Ident): T.Ident = {
    ident.scope.resolveEntry(ident.name) match {
      case Some(symbol) =>
        ???
    }
  }

  def inferTypeVarBindingIdent(ident: P.Ident, typ: Type): T.Ident = {
    ident.scope.typeSymbols.get(ident.name) match {
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        setTypeVar(symbol, typ)
        ident.scope.setTypeVar(ident.name, TypeEntry(symbol))
        T.Ident(
          meta = ident.meta.typed(typ),
          name = symbol
        )
    }
  }

  def inferVarBindingIdent(ident: P.Ident, typ: Type): T.Ident = {
    ident.scope.getSymbol(ident.name) match {
      case None =>
        val symbol = ctx.makeSymbol(ident.name)
        ident.scope.setSymbol(ident.name, ScopeEntry(symbol))
        T.Ident(
          meta = ident.meta.typed(Uninferred),
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

  def isTypeDeclarationOf(typeName: String)(decl: P.Declaration): Boolean = decl match {
    case alias: P.Declaration.TypeAlias if alias.ident.name == typeName => true
    case _: P.Declaration.TypeAlias => false
    case _: P.Declaration.Let => false
  }


  def checkModifiers(
    errors: ListBuffer[diagnostics.Diagnostic]
  )(modifiers: Iterable[P.Declaration.Modifier]) = {

  }
}
