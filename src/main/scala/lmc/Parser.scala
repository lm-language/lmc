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

  def apply(ctx: Context.Parser, path: Path, tokens: Stream[Token]) =
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

final class Parser(ctx: Context.Parser, val path: Path, val tokens: Stream[Token]) {

  type Scope = syntax.Parsed._Scope
  private var _currentToken: Token = tokens.next
  private var _lastToken: Token = _currentToken
  private var _lookahead: Option[Token] = None
  private def currentToken = _currentToken
  private var _errors: List[WeakReference[Diagnostic]] = List()
  private var _scopes = List.empty[WeakReference[Scope]]
  private var _parents = List(-1)

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

  private def buildNode[T <: Node](head: lmc.common.HasLoc = currentToken, builder: (Meta, ListBuffer[Diagnostic]) => T): T = {
    val meta = MutableMeta(
      id = ctx.nextMetaId(),
      _loc = Loc.between(currentToken, currentToken),
      scope = scope(),
      _diagnostics = ListBuffer.empty,
      _parent = _parents.head
    )
    _parents = meta.id::_parents
    val startToken = head
    val errors = ListBuffer.empty[Diagnostic]
    val result = builder(meta,  errors)
    _parents = _parents.tail
    val endToken = currentToken.previous.flatMap(_.get).getOrElse(currentToken)
    meta.setLoc(Loc.between(startToken, endToken))
    ctx.setParsedNode(result.getMeta.id, result)

    meta.setDiagnostics(errors.toArray[Diagnostic])
    result
  }

  private def withNewScope[T <: Node](f: (ScopeBuilder) => T): T = {
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
    scope.setParsedNode(result)
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
          lexeme = "",
          previous = Some(WeakReference(currentToken))
        )
    }
  }

  def parseSourceFile(): SourceFile = withNewScope((scope) => {
    buildNode(builder = (meta, errors) => {
      while (currentToken.variant == NEWLINE) {
        advance()
      }
      var declarations = Vector.empty[Declaration]
      val startToken = this.currentToken
      while (this.currentToken.variant != EOF) {
        declarations = declarations :+ this.parseDeclaration
      }
      expect(errors)(EOF)
      SourceFile(
        meta,
        declarations,
        scope = scope
      )
    })

  })

  def parseDeclaration: Declaration = buildNode(builder = (meta, errors) => {

    val modifiersWithTokens = parseModifiers(errors)
    val modifiers = modifiersWithTokens.map(_._2).toSet
    val modifierTokens = modifiersWithTokens.map(_._1)
    val startLocNode = modifierTokens.headOption.getOrElse(currentToken)
    val result = currentToken.variant match {
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
          meta = meta,
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
          meta = meta,
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
          meta = meta,
          variant = Declaration.TypeAlias(ident, kindAnnotation, rhs),
          modifiers
        )
      case ENUM =>
        val outerScope = scope()
        withNewScope((enumScope) => {
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
            meta = meta,
            modifiers = modifiers,
            variant = Declaration.Enum(
              enumScope, ident, genericParams, cases.toVector
            )
          )
        })
      case _ =>
        val startTok = currentToken

        val skippedDiagnostics = recover()
        errors.append(Diagnostic(
          DeclarationExpected(),
          Severity.Error,
          startTok.loc
        ))
        errors.appendAll(skippedDiagnostics)
        Declaration(
          meta = meta,
          variant = Declaration.Error(),
          modifiers
        )
    }
    result
  })

  private def parseEnumCase(): EnumCase = buildNode(builder = (meta, errors) => {
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
      meta,
      name,
      params
    )
  })

  private def parseModifiers(
    errors: ListBuffer[Diagnostic],
    modifiers: Vector[(Token, Declaration.Modifier)] = Vector()
  ): Vector[(Token, Declaration.Modifier)] = {
    Parser.DECL_MODIFIERS.get(currentToken.variant) match {
      case Some(modifier) =>
        val tok = advance()
        if (modifiers.map(_._2).contains(modifier)) {
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

  private def parseKindAnnotation(): KindAnnotation = buildNode(builder = (meta, errors) => {
    currentToken.variant match {
      case STAR =>
        val tok = advance()
        KindAnnotation(
          meta = meta,
          variant = KindAnnotation.Star
        )
      case LSQB =>
        val startTok = advance()
        val hd = parseKindAnnotation()
        val tl = parseCommaSeperatedListTail(() => parseKindAnnotation())
        expect(errors)(RSQB)
        expect(errors)(FATARROW)
        val to = parseKindAnnotation()
        KindAnnotation(
          meta = meta,
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
          meta = meta,
          variant = KindAnnotation.Error
        )
    }
  })

  private def parseExpr(): Expr = {
    def parseIntLiteral() = buildNode(builder = (meta, errors) => {
      val tok = advance()
      val value = tok.lexeme.replaceAll("_", "").toInt
      Expr(
        meta = meta,
        typ = (),
        variant = Expr.Literal(Expr.LInt(value))
      )
    })

    def parseFn() = buildNode(builder = (meta, errors) => {
      val parentScope = scope()
      withNewScope(fnScope => {
        val startTok = advance()
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
          meta = meta,
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
    })

    def parseVar() = buildNode(builder = (meta, errors) => {
      val ident = parseIdent()
      Expr(
        meta = meta,
        typ = (),
        variant = Expr.Var(ident)
      )
    })

    def parseModule() = buildNode(builder = (meta, errors) => {
      val tok = advance()
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
        Expr(
          meta = meta,
          typ = (),
          variant = Expr.Module(
            moduleScope,
            declarations
          )
        )
      })
    })

    def parseBlock() = buildNode(builder = (meta, errors) => {
      withNewScope(blockScope => {
        var members = Vector.empty[BlockMember]
        advance() // eat lBrace
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
        expect(errors)(RBRACE)
        Expr(
          meta,
          typ = (),
          Expr.Block(
            blockScope,
            members
          )
        )
      })
    })

    def parseIf() = buildNode(builder = (meta, errors) => {
      val ifTok = advance()
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
      Expr(
        meta, (), variant
      )
    })

    def parseMatchBranch(): Expr.MatchBranch =
      buildNode(builder = (meta, errors) =>
        withNewScope(branchScope => {
          val p = parsePattern()
          expect(errors)(FATARROW)
          val e = parseExpr()
          Expr.MatchBranch(meta, branchScope, p, e)
        })
      )

    def parseMatch() = buildNode(builder = (meta, errors) => {
      advance()
      val e = parseExpr()
      expect(errors)(LBRACE)
      val branches = parseCommaSeperatedList(parseMatchBranch)(Parser.PATTERN_PREDICTORS)
      expect(errors)(RBRACE)
      Expr(
        meta,
        (),
        Expr.Match(e, branches.toVector)
      )
    })
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
        buildNode(builder = (meta, errors) => {
          val loc = currentToken.loc
          val skippedDiagnostics = recover()
          errors.appendAll(skippedDiagnostics)
          errors.append(Diagnostic(
            ExpressionExpected(),
            Severity.Error,
            loc
          ))
          Expr(
            meta = meta,
            typ = (),
            variant = Expr.Error()
          )
        })
    }
    ctx.setParsedNode(head.meta.id, head)
    val result = parseExprTail(head)
    ctx.setParsedNode(result.meta.id, result)
    result
  }

  private def parseExprTail(head: Expr): Expr = buildNode(head, (meta, errors) => {
    currentToken.variant match {
      case LPAREN =>
        val lparen = advance()
        val args = parseCommaSeperatedList(() => parseArg())(Parser.ARG_PREDICTORS).toVector
        val rparen = expect(errors)(RPAREN)
        Expr(
          meta = meta,
          typ = (),
          variant = Expr.Call(Loc.between(lparen, rparen), head, args)
        )
      case DOT =>
        advance()
        val propTok = expect(errors)(ID)
        parseExprTail(Expr(
          meta = meta,
          typ = (),
          variant = Expr.Prop(head, propTok.lexeme)
        ))
      case WITH =>
        advance()
        val e2 = parseExpr()
        parseExprTail(Expr(
          meta = meta,
          typ = (),
          variant = Expr.WithExpression(head, e2)
        ))
      case _ => head
    }
  })

  private def parseArg(): Expr.Arg = {
    val errors = ListBuffer.empty[Diagnostic]
    peek.variant match {
      case EQ =>
        val labelTok = expect(errors)(ID)
        advance() // eat EQ
        val expr = parseExpr()
        Expr.Arg(Some(labelTok, labelTok.lexeme), expr)
      case _ =>
        val e = parseExpr()
        Expr.Arg(None, e)
    }
  }

  private def parsePattern(): Pattern = buildNode(builder = (meta, errors) => {
    val head = currentToken.variant match {
      case ID =>

        val ident = parseIdent()
        Pattern(
          meta = meta,
          typ = (),
          variant = Pattern.Var(ident)
        )
      case LPAREN =>
        advance()
        val p = parsePattern()
        expect(errors)(RPAREN)
        Pattern(
          meta = meta,
          typ = p.typ,
          variant = p.variant
        )
      case DOT =>
        val firstTok = advance()
        val ident = parseIdent()
        val lastTok = ident
        Pattern(
          meta = meta,
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
          meta = meta,
          typ = (),
          variant = Pattern.Error
        )
    }
    ctx.setParsedNode(head.meta.id, head)
    val result = currentToken.variant match {
      case COLON =>
        advance()
        val annotation = parseTypeAnnotation()
        Pattern(
          meta,
          typ = (),
          variant = Pattern.Annotated(head, annotation)
        )
      case LPAREN =>
        advance()
        val params = parseCommaSeperatedList(parsePatternParam)(
          Parser.PATTERN_PARAM_PREDICTORS
        )
        val rparen = expect(errors)(RPAREN)
        Pattern(
          meta,
          typ = (),
          variant = Pattern.Function(head, params.toVector)
        )
      case _ => head
    }
    result
  })

  private def parsePatternParam():
    Pattern.Param = buildNode(builder = (meta, errors) => {
    val result = currentToken.variant match {
      case DOTDOT =>
        val tok = currentToken
        advance()
        Pattern.Param.Rest(meta)
      case _ =>
        peek.variant match {
          case EQ =>
            val ident = parseIdent()
            expect(errors)(EQ)
            val pattern = parsePattern()
            Pattern.Param.SubPattern(meta, Some(ident), pattern)
          case _ =>
            val p = parsePattern()
            Pattern.Param.SubPattern(meta, None, p)
        }

    }
    ctx.setParsedNode(result.getMeta.id, result)
    result
  })

  private def parseTypeAnnotation(): TypeAnnotation = buildNode(builder = (meta, errors) => {
     val head = currentToken.variant match {
       case LPAREN =>
         advance()
         val result = this.parseTypeAnnotation()
         expect(errors)(RPAREN)
         result.copy(
           meta = meta.withDiagnostics(result.meta.diagnostics)
         )
       case LSQB =>
         withNewScope((scope) => {
           val firstTok = advance()
           val genericParams = parseGenericParamsList()
           expect(errors)(RSQB)
           expect(errors)(FATARROW)
           val annotation = parseTypeAnnotation()
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
         expect(errors)(LPAREN)
         val params = parseCommaSeperatedList(() => {
           val label = peek.variant match {
             case COLON =>
               val labelTok = parseIdent()
               advance() // consume colon
               Some(parseIdent())
             case _ =>
               None
           }
           val typ = parseTypeAnnotation()
           (label, typ)
         })(Parser.TYPE_PREDICTORS)
         expect(errors)(RPAREN)
         expect(errors)(FATARROW)
         val returnType = parseTypeAnnotation()
         TypeAnnotation(meta, (), TypeAnnotation.Func(params, returnType))
       case v if Parser.EXPR_PREDICTORS.contains(v) =>
         val e = parseExpr()
         e.variant match {
           case Expr.Var(ident) =>
             TypeAnnotation(meta, (), TypeAnnotation.Var(ident))
           case Expr.Prop(e, prop) =>
             TypeAnnotation(
               meta,
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
           meta = meta,
           kind = (),
           variant = TypeAnnotation.Error
         )
     }
    ctx.setParsedNode(head.meta.id, head)
    val result = parseTypeAnnotationTail(head)
    ctx.setParsedNode(result.meta.id, result)
    result
  })

  private def parseTypeAnnotationTail(head: TypeAnnotation): TypeAnnotation = buildNode(head, (meta, errors) => {
    currentToken.variant match {
      case LSQB =>
        advance()
        val hd = parseTypeAnnotation()
        val tl = parseCommaSeperatedListTail(() => parseTypeAnnotation())
        val args = hd::tl
        expect(errors)(RSQB)
        parseTypeAnnotationTail(
          TypeAnnotation(
            meta = meta,
            kind = (),
            variant = TypeAnnotation.TApplication(head, args)
          )
        )
      case _ => head
    }
  })

  private def parseParam(): Expr.Param = {
    val pattern = parsePattern()
    Expr.Param(pattern)
  }

  private def parseGenericParamsList(): Iterable[GenericParam] = {
    val hd = parseGenericParam()
    val tl = parseCommaSeperatedListTail(() => parseGenericParam())
    hd::tl
  }

  private def parseGenericParam(): GenericParam = buildNode(builder = (meta, errors) => {
    val ident = parseIdent()
    val kindAnnotation = currentToken.variant match {
      case COLON =>
        advance()
        Some(parseKindAnnotation())
      case _ => None
    }

    GenericParam(meta, (), ident, kindAnnotation)
  })

  private def parseIdent(): Ident = buildNode(builder = (meta, errors) => {
    val tok = expect(errors)(ID)
    val result = Ident(
      meta,
      tok.lexeme
    )
    ctx.setParsedNode(result.meta.id, result)
    result
  })

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