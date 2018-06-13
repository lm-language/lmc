package lmc

import lmc.diagnostics.Diagnostic
import lmc.types._

import scala.collection.mutable.ListBuffer


final class TypeChecker(
  private val ctx: Context.TC
) {
  import lmc.syntax.{Parsed => P, Typed => T}

  private val Primitive = ctx.Primitive

  private val _checkedDecls = collection.mutable.WeakHashMap.empty[Int, T.Declaration]

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
        val inferredAnnotation = inferAnnotation(annotation)
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

    }
  }

  def inferExpr(expr: P.Expression): T.Expression = {
    ???
  }

  def inferPattern(pattern: P.Pattern): T.Pattern = {
    ???
  }

  def checkExpr(typ: Type)(expr: P.Expression): T.Expression = {
    ???
  }

  def checkPattern(typ: Type)(pattern: P.Pattern): T.Pattern = {
    ???
  }

  def inferAnnotation(annotation: P.TypeAnnotation): T.TypeAnnotation = {
    annotation match {
      case P.TypeAnnotation.Var(meta, ident) =>
        val inferredIdent = inferTypeVarIdent(ident)
        T.TypeAnnotation.Var(
          meta.typed(inferredIdent.meta.typ),
          inferredIdent
        )

    }
  }

  def inferTypeVarIdent(ident: P.Ident): T.Ident = {
    getTypeDeclOf(ident.name, ident) match {
      case Some(decl) =>
        ???
    }
  }

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
    case _: P.Declaration.Let => false
  }


  def checkModifiers(
    errors: ListBuffer[diagnostics.Diagnostic]
  )(modifiers: Iterable[P.Declaration.Modifier]) = {

  }
}
