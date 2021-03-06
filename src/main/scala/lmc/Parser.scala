package lmc

import java.nio.file.Path

import syntax.token
import lmc.syntax.Parsed._
import token.Token
import token.Variant._
import diagnostics._
import io.Stream
import common.Loc
import common.ScopeBuilder

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

object Parser {

  def apply(ctx: Context, path: Path, tokens: Stream[Token]) =
    new Parser(ctx, path, tokens)
  val RECOVERY_TOKENS = Set(
    NEWLINE, EOF, RPAREN, RBRACE, SEMICOLON
  )

  def isRecoveryToken(variant: token.Variant): Boolean = {
    RECOVERY_TOKENS.contains(variant)
  }
  val EXPR_PREDICTORS: Set[token.Variant] = Set(
    ID, LPAREN, FN, LBRACE, INT, MODULE
  )

  val PATTERN_PREDICTORS: Set[token.Variant] = Set(ID, LPAREN, DOT)
  val PARAM_PREDICTORS: Set[token.Variant] = PATTERN_PREDICTORS
  val PATTERN_PARAM_PREDICTORS: Set[token.Variant] = PATTERN_PREDICTORS union Set(DOTDOT)

  val TYPE_PREDICTORS: Set[token.Variant] = Set(ID, LPAREN, LSQB).union(EXPR_PREDICTORS)

  val ARG_PREDICTORS: Set[token.Variant] = EXPR_PREDICTORS
  val DECL_MODIFIERS: Map[token.Variant, Declaration.Modifier] = Map(
    EXTERN -> Declaration.Modifier.Extern,
    ABSTRACT -> Declaration.Modifier.Abstract,
    OVERRIDE -> Declaration.Modifier.Override
  )
  val DECL_PREDICTORS: Set[token.Variant] = Set(
    LET, EXTERN, TYPE, INCLUDE, ENUM
  ) union DECL_MODIFIERS.keySet
}

final class Parser(ctx: Context, val path: Path, val tokens: Stream[Token]) {

  type Scope = syntax.Parsed._Scope
  private var _currentToken: Token = tokens.next
  private var _lastToken: Token = _currentToken
  private var _lookahead: Option[Token] = None
  private def currentToken = _currentToken
  private var _errors: List[WeakReference[Diagnostic]] = List()
  private var _scopes = List.empty[WeakReference[Scope]]

  /**
   * advance the token stream by one character
   * and return the currentToken before advancing
   */
  private def advance(): Token = {
    _lookahead match {
      case Some(tok) =>
        val result = _currentToken
        _currentToken = tok
        _lookahead = None
        result
      case None =>
        val tok = currentToken
        _lastToken = _currentToken
        _currentToken = tokens.next
        while (currentToken.variant == NEWLINE) {
          _currentToken = tokens.next
        }
        tok
    }
  }

  private def peek: Token = {
    _lookahead match {
      case Some(tok) =>
        tok
      case None =>
        val tok = tokens.next
        _lookahead = Some(tok)
        tok
    }
  }

  private def withNewScope[T](f: (ScopeBuilder) => T): T = {
    val startToken = currentToken
    val parent: Option[ScopeBuilder] = _scopes match {
      case hd :: _ =>
        hd.get
      case _ =>
        None
    }

    val scope = common.ScopeBuilder(parent match {
      case Some(p) => Some(WeakReference(p))
      case None => Some(WeakReference(ctx.PrimitiveScope))
    })
    parent match {
      case Some(parentScope) =>
        parentScope.addChild(scope)
      case None =>
        ()
    }
    _pushScope(scope)
    val result = f(scope)
    _popScope()
    val loc = Loc.between(startToken, _lastToken)
    scope.setLoc(loc)
    result
  }

  private def _pushScope(scope: Scope): Unit = {
    this._scopes = WeakReference(scope)::_scopes
  }

  private def _popScope(): Unit = {
    _scopes = _scopes.tail
  }

  private def scope(): WeakReference[Scope] = {
    // Unsafe get because scope should definitely be
    // defined during parsing, otherwise, something
    // is wrong and exception should be thrown
    WeakReference(_scopes.head.get.get)
  }

  private def expect(errors: collection.mutable.ListBuffer[Diagnostic])
      (variant: token.Variant): Token = {
    currentToken.variant match {
      case v if v == variant =>
        advance()
      case _ =>
        val loc = currentToken.loc.copy(end = currentToken.loc.start)
        advance()
        errors.append(
          Diagnostic(
            severity = diagnostics.Severity.Error,
            loc = loc,
            variant = diagnostics.TokenExpected(variant.toString)))
        Token(
          loc = loc,
          variant = EXPECTED,
          lexeme = ""
        )
    }
  }

  def parseSourceFile(): SourceFile = withNewScope((scope) => {
    while (currentToken.variant == NEWLINE) {
      advance()
    }
    var declarations = Vector.empty[Declaration]
    val startToken = this.currentToken
    while (this.currentToken.variant != EOF) {
      declarations = declarations :+ this.parseDeclaration
    }
    val errors = ListBuffer.empty[Diagnostic]
    val endToken = expect(errors)(EOF)
    val loc = Loc.between(startToken, endToken)
    val meta = makeMeta(
      loc,
      WeakReference(scope),
      errors.toList
    )
    SourceFile(
      meta,
      declarations,
      scope = scope
    )
  })

  private def makeMeta(
    loc: Loc,
    scope: WeakReference[Scope],
    diagnostics: Iterable[Diagnostic] = List.empty
  ): Meta =
    Meta(
      loc = loc,
      scope = scope,
      id = ctx.nextMetaId(),
      diagnostics = diagnostics
    )

  def parseDeclaration: Declaration = {

    val errors = collection.mutable.ListBuffer.empty[Diagnostic]
    val modifiersWithTokens = parseModifiers(errors)
    val modifiers = modifiersWithTokens.map(_._2).toSet
    val modifierTokens = modifiersWithTokens.map(_._1)
    val startLocNode = modifierTokens.headOption.getOrElse(currentToken)
    currentToken.variant match {
      case SEMICOLON =>
        advance()
        parseDeclaration
      case LET =>
        advance()
        val pattern = parsePattern()
        val rhs = currentToken.variant match {
          case EQ =>
            expect(errors)(EQ)
            val expr = parseExpr()
            Some(expr)
          case _ => None
        }
        expect(errors)(SEMICOLON)
        Declaration(
          meta = makeMeta(
            loc = Loc.between(startLocNode, rhs.getOrElse(pattern)),
            scope = scope(),
            diagnostics = errors.toList
          ),
          variant = Declaration.Let(pattern, rhs),
          modifiers
        )
      case INCLUDE =>
        advance()
        val e = parseExpr()
        expect(errors)(SEMICOLON)
        for (modifierTok <- modifierTokens) {
          errors.append(
            Diagnostic(
              loc = modifierTok.loc,
              severity = Severity.Error,
              variant = ModifierOnInclude
            )
          )
        }
        Declaration(
          meta = makeMeta(
            loc = Loc.between(startLocNode, e),
            scope = scope(),
            diagnostics = errors.toList
          ),
          variant = Declaration.Include(e),
          modifiers
        )
      case TYPE =>
        advance()
        val ident = parseIdent()
        val kindAnnotation = currentToken.variant match {
          case COLON =>
            advance()
            Some(parseKindAnnotation())
          case _ => None
        }
        val rhs = currentToken.variant match {
          case EQ =>
            expect(errors)(EQ)
            val typ = parseTypeAnnotation()
            Some(typ)
          case _ => None
        }
        val semicolon = expect(errors)(SEMICOLON)
        Declaration(
          meta = makeMeta(
            loc = Loc.between(startLocNode, semicolon),
            diagnostics = errors.toList,
            scope = scope()
          ),
          variant = Declaration.TypeAlias(ident, kindAnnotation, rhs),
          modifiers
        )
      case ENUM =>
        val outerScope = scope()
        withNewScope((enumScope) => {
          val errors = ListBuffer.empty[Diagnostic]
          val firstTok = advance()
          val ident = parseIdent()
          val genericParams = currentToken.variant match {
            case LSQB =>
              advance()
              val params = parseGenericParamsList().toVector
              expect(errors)(RSQB)
              params
            case _ => Vector.empty
          }
          expect(errors)(LBRACE)
          val cases = ListBuffer.empty[EnumCase]
          while (!(Set(EOF, RBRACE) contains currentToken.variant)) {
            cases.append(parseEnumCase())
          }
          val rbrace = expect(errors)(RBRACE)
          expect(errors)(SEMICOLON)
          Declaration(
            meta = makeMeta(
              Loc.between(firstTok, rbrace),
              scope = outerScope,
              errors.toVector
            ),
            modifiers = modifiers,
            variant = Declaration.Enum(
              enumScope, ident, genericParams, cases.toVector
            )
          )
        })
      case _ =>
        val startTok = currentToken

        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          DeclarationExpected(),
          Severity.Error,
          startTok.loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        Declaration(
          meta = makeMeta(
            loc = Loc.between(
              modifierTokens.headOption.getOrElse(startTok),
              currentToken
            ),
            scope(),
            diagnostics ++ skippedDiagnostics
          ),
          variant = Declaration.Error(),
          modifiers
        )
    }
  }

  private def parseEnumCase(): EnumCase = {
    val errors = ListBuffer.empty[Diagnostic]
    val name = parseIdent()
    val params: Vector[(Option[String], TypeAnnotation)] = currentToken.variant match {
      case LPAREN =>
        advance()
        val result = parseCommaSeperatedList(() =>
          peek.variant match {
            case COLON =>
              val idTok = expect(errors)(ID)
              expect(errors)(COLON)
              (Some(idTok.lexeme), parseTypeAnnotation())
            case _ =>
              (None, parseTypeAnnotation())
          }
        )(Set(ID))
        expect(errors)(RPAREN)
        result.toVector
      case _ =>
        Vector.empty
    }
    expect(errors)(SEMICOLON)
    EnumCase(
      makeMeta(
        Loc.between(name, params.lastOption.map(_._2).getOrElse(name)),
        scope(),
        errors.toList
      ),
      name,
      params
    )

  }

  private def parseModifiers(
    errors: ListBuffer[Diagnostic],
    modifiers: Vector[(Token, Declaration.Modifier)] = Vector()
  ): Vector[(Token, Declaration.Modifier)] = {
    Parser.DECL_MODIFIERS.get(currentToken.variant) match {
      case Some(modifier) =>
        val tok = advance()
        if (modifiers.contains(modifier)) {
          errors.append(
            Diagnostic(
              loc = tok.loc,
              severity = Severity.Error,
              variant = DuplicateModifier(tok.lexeme)
            )
          )
          parseModifiers(errors, modifiers)
        } else {
          parseModifiers(errors, modifiers ++ Vector(tok -> modifier))
        }
      case _ => modifiers
    }
  }

  private def parseKindAnnotation(): KindAnnotation = {
    currentToken.variant match {
      case STAR =>
        val tok = advance()
        KindAnnotation(
          meta = makeMeta(loc = tok.loc, scope = scope(), List.empty),
          variant = KindAnnotation.Star
        )
      case LSQB =>
        val startTok = advance()
        val hd = parseKindAnnotation()
        val tl = parseCommaSeperatedListTail(() => parseKindAnnotation())
        val errors = ListBuffer.empty[Diagnostic]
        expect(errors)(RSQB)
        expect(errors)(FATARROW)
        val to = parseKindAnnotation()
        KindAnnotation(
          meta = makeMeta(loc = Loc.between(startTok, to), scope = scope(), List.empty),
          variant = KindAnnotation.KFun(hd::tl, to)
        )
      case _ =>
        val loc = currentToken.loc
        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          KindExpected,
          Severity.Error,
          loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        KindAnnotation(
          meta = makeMeta(
            loc = currentToken.loc,
            scope(),
            diagnostics ++ skippedDiagnostics
          ),
          variant = KindAnnotation.Error
        )
    }
  }

  private def parseExpr(): Expr = {
    def parseIntLiteral() = {
      val tok = advance()
      val value = tok.lexeme.replaceAll("_", "").toInt
      Expr(
        meta = makeMeta(loc = tok.loc, scope()),
        typ = (),
        variant = Expr.Literal(Expr.LInt(value))
      )
    }

    def parseFn() = {
      val parentScope = scope()
      withNewScope(fnScope => {
        val startTok = advance()
        val errors = collection.mutable.ListBuffer.empty[Diagnostic]
        val genericParams = currentToken.variant match {
          case LSQB =>
            advance()
            val params = parseGenericParamsList()
            expect(errors)(RSQB)
            params
          case _ => List()
        }
        expect(errors)(LPAREN)
        val params = parseCommaSeperatedList(() => parseParam())(Parser.PATTERN_PREDICTORS)
        expect(errors)(RPAREN)
        val annotation = currentToken.variant match {
          case COLON =>
            advance()
            Some(parseTypeAnnotation())
          case _ => None
        }
        expect(errors)(FATARROW)
        val body = parseExpr()
        Expr(
          meta = makeMeta(
            loc = Loc.between(startTok, body),
            scope = parentScope,
            errors.toList
          ),
          typ = (),
          variant = Expr.Func(
            startTok,
            fnScope,
            genericParams.toVector,
            params,
            annotation,
            body
          )
        )
      })
    }

    def parseVar() = {
      val tok = advance()
      val meta = makeMeta(loc = tok.loc, scope())
      Expr(
        meta = meta,
        typ = (),
        variant = Expr.Var(Ident(meta, tok.lexeme))
      )
    }

    def parseModule() = {
      val tok = advance()
      val errors = ListBuffer.empty[Diagnostic]
      withNewScope((moduleScope) => {
        expect(errors)(LBRACE)
        val declarations = {
          val buffer = ListBuffer.empty[Declaration]
          while (Parser.DECL_PREDICTORS.contains(currentToken.variant)) {
            buffer.append(this.parseDeclaration)
          }
          buffer.toVector
        }
        val rbrace = expect(errors)(RBRACE)
        val meta = makeMeta(loc = Loc.between(tok, rbrace), scope = scope())
        Expr(
          meta = meta,
          typ = (),
          variant = Expr.Module(
            moduleScope,
            declarations
          )
        )
      })
    }

    def parseBlock() = withNewScope(blockScope => {
      val errors = ListBuffer.empty[Diagnostic]
      var members = Vector.empty[BlockMember]
      val lbrace = advance()
      while (currentToken.variant != RBRACE && currentToken.variant != EOF) {
        val member = if (Parser.DECL_PREDICTORS.contains(currentToken.variant)) {
          parseDeclaration
        } else {
          val e = parseExpr()
          expect(errors)(SEMICOLON)
          e
        }
        members = members.:+(member)
      }
      val rbrace = expect(errors)(RBRACE)

      val meta = makeMeta(Loc.between(lbrace, rbrace), scope(), errors.toList)
      Expr(
        meta,
        typ = (),
        Expr.Block(
          blockScope,
          members
        )
      )
    })

    def parseIf() = {
      val ifTok = advance()
      val errors = ListBuffer.empty[Diagnostic]
      expect(errors)(LPAREN)
      val predicate = parseExpr()
      expect(errors)(RPAREN)
      val trueBranch = parseExpr()
      val falseBranch = currentToken.variant match {
        case ELSE =>
          advance()
          Some(parseExpr())
        case _ => None
      }
      val variant = Expr.If(
        predicate, trueBranch, falseBranch
      )
      val meta = makeMeta(
        Loc.between(ifTok, falseBranch.getOrElse(trueBranch)),
        scope()
      )
      Expr(
        meta, (), variant
      )
    }

    def parseMatchBranch(errors: ListBuffer[Diagnostic])(): Expr.MatchBranch = withNewScope(scope => {
      val p = parsePattern()
      expect(errors)(FATARROW)
      val e = parseExpr()
      Expr.MatchBranch(
        scope,
        p,
        e
      )
    })

    def parseMatch() = {
      val errors = ListBuffer.empty[Diagnostic]
      val matchTok = advance()
      val e = parseExpr()
      expect(errors)(LBRACE)
      val branches = parseCommaSeperatedList(parseMatchBranch(errors))(Parser.PATTERN_PREDICTORS)
      val rbrace = expect(errors)(RBRACE)
      val meta = makeMeta(
        loc = Loc.between(matchTok, rbrace),
        scope = scope(),
        errors.toList
      )
      Expr(
        meta,
        (),
        Expr.Match(e, branches.toVector)
      )
    }
    val head = currentToken.variant match {
      case INT =>
        parseIntLiteral()
      case FN =>
        parseFn()
      case ID =>
        parseVar()
      case MODULE =>
        parseModule()
      case LBRACE =>
        parseBlock()
      case IF =>
        parseIf()
      case MATCH =>
        parseMatch()
      case _ =>
        val loc = currentToken.loc
        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          ExpressionExpected(),
          Severity.Error,
          loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        Expr(
          meta = makeMeta(
            loc = currentToken.loc,
            scope(),
            diagnostics ++ skippedDiagnostics
          ),
          typ = (),
          variant = Expr.Error()
        )
    }
    parseExprTail(head)
  }

  private def parseExprTail(head: Expr): Expr = {
    currentToken.variant match {
      case LPAREN =>
        val errors = ListBuffer.empty[Diagnostic]
        val lparen = advance()
        val args = parseCommaSeperatedList(() => parseArg())(Parser.ARG_PREDICTORS).toVector
        val rparen = expect(errors)(RPAREN)
        Expr(
          meta = makeMeta(
            scope = head.meta.scope,
            loc = Loc.between(head, rparen),
            diagnostics = errors
          ),
          typ = (),
          variant = Expr.Call(Loc.between(lparen, rparen), head, args)
        )
      case DOT =>
        val errors = ListBuffer.empty[Diagnostic]
        advance()
        val propTok = expect(errors)(ID)
        parseExprTail(Expr(
          meta = makeMeta(
            loc = Loc.between(head, propTok),
            scope = head.meta.scope,
            diagnostics = errors.toList
          ),
          typ = (),
          variant = Expr.Prop(head, propTok.lexeme)
        ))
      case WITH =>
        val errors = ListBuffer.empty[Diagnostic]
        advance()
        val e2 = parseExpr()
        parseExprTail(Expr(
          meta = makeMeta(
            loc = Loc.between(head, e2),
            scope = scope()
          ),
          typ = (),
          variant = Expr.WithExpression(head, e2)
        ))
      case _ => head
    }
  }

  private def parseArg(): Expr.Arg = {
    peek.variant match {
      case EQ =>
        val errors = ListBuffer.empty[Diagnostic]
        val labelTok = expect(errors)(ID)
        advance() // eat EQ
        val expr = parseExpr()
        Expr.Arg(Some(labelTok, labelTok.lexeme), expr)
      case _ =>
        val e = parseExpr()
        Expr.Arg(None, e)
    }
  }

  private def parsePattern(): Pattern = {
    val head = currentToken.variant match {
      case ID =>
        val tok = advance()
        val meta = makeMeta(loc = tok.loc, scope())
        Pattern(
          meta = meta,
          typ = (),
          variant = Pattern.Var(Ident(meta, tok.lexeme))
        )
      case LPAREN =>
        advance()
        val p = parsePattern()
        val errors = collection.mutable.ListBuffer.empty[Diagnostic]
        expect(errors)(RPAREN)
        p.copy(meta = p.meta.copy(diagnostics = p.meta.diagnostics ++ errors))

      case DOT =>
        val firstTok = advance()
        val ident = parseIdent()
        val lastTok = ident
        Pattern(
          meta = makeMeta(
            Loc.between(firstTok, lastTok),
            scope = scope()
          ),
          typ = (),
          variant = Pattern.DotName(ident)
        )
      case _ =>
        val loc = currentToken.loc

        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          PatternExpected(),
          Severity.Error,
          loc
        ))
        addDiagnostics(diagnostics)
        addDiagnostics(skippedDiagnostics)
        Pattern(
          meta = makeMeta(
            loc = currentToken.loc,
            scope(),
            diagnostics ++ skippedDiagnostics
          ),
          typ = (),
          variant = Pattern.Error
        )
    }
    currentToken.variant match {
      case COLON =>
        advance()
        val annotation = parseTypeAnnotation()
        Pattern(
          meta = makeMeta(
            loc = Loc.between(head, annotation),
            scope()
          ),
          typ = (),
          variant = Pattern.Annotated(head, annotation)
        )
      case LPAREN =>
        advance()
        val errors = ListBuffer.empty[Diagnostic]
        val params = parseCommaSeperatedList(parsePatternParam(errors))(
          Parser.PATTERN_PARAM_PREDICTORS
        )
        val rparen = expect(errors)(RPAREN)
        Pattern(
          meta = makeMeta(
            loc = Loc.between(head, rparen),
            scope(),
            errors.toList
          ),
          typ = (),
          variant = Pattern.Function(head, params.toVector)
        )
      case _ => head
    }
  }

  private def parsePatternParam(errors: ListBuffer[Diagnostic])(): Pattern.Param = {
    currentToken.variant match {
      case DOTDOT =>
        val tok = currentToken
        advance()
        Pattern.Param.Rest(makeMeta(tok.loc, scope(), List()))
      case _ =>
        peek.variant match {
          case EQ =>
            val ident = parseIdent()
            expect(errors)(EQ)
            val pattern = parsePattern()
            val meta = makeMeta(Loc.between(ident, pattern), scope(), List())
            Pattern.Param.SubPattern(meta, Some(ident), pattern)
          case _ =>
            val p = parsePattern()
            val meta = makeMeta(p.loc, scope(), List())
            Pattern.Param.SubPattern(meta, None, p)
        }

    }
  }

  private def parseTypeAnnotation(): TypeAnnotation = {
     val head = currentToken.variant match {
       case LPAREN =>
         advance()
         val result = this.parseTypeAnnotation()
         val errors = ListBuffer.empty[Diagnostic]
         expect(errors)(RPAREN)
         result.copy(
           meta = result.meta.copy(
             diagnostics = result.meta.diagnostics ++ errors
           )
         )
       case LSQB =>
         withNewScope((scope) => {
           val errors = ListBuffer.empty[Diagnostic]
           val firstTok = advance()
           val genericParams = parseGenericParamsList()
           expect(errors)(RSQB)
           expect(errors)(FATARROW)
           val annotation = parseTypeAnnotation()
           val meta = makeMeta(
             loc = Loc.between(firstTok, annotation),
             diagnostics = errors,
             scope = this.scope()
           )
           TypeAnnotation(
             meta = meta,
             kind = (),
             variant = TypeAnnotation.Forall(
               scope, genericParams, annotation
             )
           )
         })
       case FN =>
         val startTok = advance()
         val errors = ListBuffer.empty[Diagnostic]
         expect(errors)(LPAREN)
         val params = parseCommaSeperatedList(() => {
           val label = peek.variant match {
             case COLON =>
               val labelErrs = ListBuffer.empty[Diagnostic]
               val labelTok = expect(labelErrs)(ID)
               advance() // consume colon
               val meta = makeMeta(
                 loc = labelTok.loc,
                 diagnostics = labelErrs,
                 scope = scope()
               )
               Some(Ident(
                 meta = meta,
                 labelTok.lexeme
               ))
             case _ =>
               None
           }
           val typ = parseTypeAnnotation()
           (label, typ)
         })(Parser.TYPE_PREDICTORS)
         expect(errors)(RPAREN)
         expect(errors)(FATARROW)
         val returnType = parseTypeAnnotation()
         val meta = makeMeta(
           loc = Loc.between(startTok, returnType),
           scope = scope(),
           errors.toList
         )
         TypeAnnotation(meta, (), TypeAnnotation.Func(params, returnType))
       case v if Parser.EXPR_PREDICTORS.contains(v) =>
         val e = parseExpr()
         e.variant match {
           case Expr.Var(ident) =>
             val meta = makeMeta(
               loc = ident.loc,
               scope = scope()
             )
             TypeAnnotation(meta, (), TypeAnnotation.Var(ident))
           case Expr.Prop(e, prop) =>
             TypeAnnotation(
               makeMeta(
                 loc = e.loc,
                 scope = scope()
               ),
               kind = (),
               variant = TypeAnnotation.Prop(
                 e, prop
               )
             )
           case _ =>
             TypeAnnotation(
               meta = e.meta.withDiagnostic(
                 Diagnostic(
                   variant = TypeExpected(),
                   severity = Severity.Error,
                   loc = e.loc
                 )
               ),
               kind = (),
               variant = TypeAnnotation.Error
             )
         }
       case _ =>
         val loc = currentToken.loc
         val skippedDiagnostics = recover()
         val diagnostics = List(Diagnostic(
            TypeExpected(),
            Severity.Error,
            loc
         ))
         addDiagnostics(diagnostics)
         addDiagnostics(skippedDiagnostics)
         TypeAnnotation(
           meta = makeMeta(
             loc = currentToken.loc,
             scope(),
             diagnostics ++ skippedDiagnostics
           ),
           kind = (),
           variant = TypeAnnotation.Error
         )
     }
    parseTypeAnnotationTail(head)
  }

  private def parseTypeAnnotationTail(head: TypeAnnotation): TypeAnnotation = {
    currentToken.variant match {
      case LSQB =>
        val errors = ListBuffer.empty[Diagnostic]
        advance()
        val hd = parseTypeAnnotation()
        val tl = parseCommaSeperatedListTail(() => parseTypeAnnotation())
        val args = hd::tl
        val rsqb = expect(errors)(RSQB)
        parseTypeAnnotationTail(
          TypeAnnotation(
            meta = makeMeta(
              loc = Loc.between(head, rsqb),
              scope = scope()
            ),
            kind = (),
            variant = TypeAnnotation.TApplication(head, args)
          )
        )
      case _ => head
    }
  }

  private def parseParam(): Expr.Param = {
    val pattern = parsePattern()
    Expr.Param(pattern)
  }

  private def parseGenericParamsList(): Iterable[GenericParam] = {
    val hd = parseGenericParam()
    val tl = parseCommaSeperatedListTail(() => parseGenericParam())
    hd::tl
  }

  private def parseGenericParam(): GenericParam = {
    val ident = parseIdent()
    val kindAnnotation = currentToken.variant match {
      case COLON =>
        advance()
        Some(parseKindAnnotation())
      case _ => None
    }

    val meta = makeMeta(
      loc = ident.loc,
      diagnostics = List.empty,
      scope = scope()
    )
    GenericParam(meta, (), ident, kindAnnotation)
  }

  private def parseIdent(): Ident = {
    val errors = ListBuffer.empty[Diagnostic]
    val tok = expect(errors)(ID)
    Ident(
      makeMeta(
        loc = tok.loc,
        scope = scope(),
        diagnostics = errors
      ),
      tok.lexeme
    )
  }

  private def parseCommaSeperatedList[T](f: (() => T))(predictors: Set[token.Variant]): List[T] = {
    if (predictors.contains(currentToken.variant)) {
      val hd = f()
      val tl = parseCommaSeperatedListTail(f)
      hd::tl
    } else {
      List.empty
    }

  }

  private def parseCommaSeperatedListTail[T](f: (() => T)): List[T] = {
    val lst = ListBuffer.empty[T]
    while (currentToken.variant == COMMA) {
      advance()
      lst.append(f())
    }
    lst.toList
  }

  private def addDiagnostics(diagnostics: Iterable[Diagnostic]): Unit = {
    for (d <- diagnostics) {
      _errors = new WeakReference[Diagnostic](d)::_errors
    }
  }

  private def recover(): List[Diagnostic] = {
    var diagnostics = List.empty[Diagnostic]
    diagnostics = consTokenDiagnostics(diagnostics, advance())
    while (!Parser.isRecoveryToken(currentToken.variant)) {
      val tok = advance()
      diagnostics = consTokenDiagnostics(diagnostics, tok)
    }
    diagnostics.reverse
  }

  private def consTokenDiagnostics(diagnostics: List[Diagnostic], tok: Token): List[Diagnostic] = {
    tok.variant match {
      case UNEXPECTED_CHAR =>
        Diagnostic(
          severity = Severity.Error,
          loc = tok.loc,
          variant = UnexpectedChar(tok.lexeme)
        )::diagnostics
      case INVALID_OPERATOR =>
          Diagnostic(
            severity = Severity.Error,
            loc = tok.loc,
            variant = InvalidOperator(tok.lexeme)
          )::diagnostics
      case _ => diagnostics
    }
  }
}