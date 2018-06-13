package lmc

import java.nio.file.Path

import syntax.token
import lmc.syntax.Parsed._
import token.Token
import token.Variant._
import diagnostics._
import io.Stream
import common.{ Loc, ScopeBuilder }

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
  private var _parents = List.empty[Int]

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


  val EMPTY_SCOPE = ScopeBuilder(None)
  private def scope(): WeakReference[Scope] = {
    // Unsafe get because scope should definitely be
    // defined during parsing, otherwise, something
    // is wrong and exception should be thrown
    WeakReference(_scopes.headOption.flatMap(_.get).getOrElse(EMPTY_SCOPE))
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

  def parseSourceFile(): SourceFile = buildNode(
    (meta, errors) => withNewScope(scope => {
       while (currentToken.variant == NEWLINE) {
         advance()
       }
       var declarations = Vector.empty[Declaration]
       val startToken = this.currentToken
       while (this.currentToken.variant != EOF) {
         declarations = declarations :+ this.parseDeclaration
       }
       val endToken = expect(errors)(EOF)
       val loc = Loc.between(startToken, endToken)
       SourceFile(
         meta,
         scope,
         declarations.toArray
       )
    })
  )

  private def buildNode[N <: Node](
    f: (Meta, ListBuffer[Diagnostic]) => N,
    start: Option[Node] = None
  ): N = {
    val errors = ListBuffer.empty[Diagnostic]
    val meta = MetaBuilder(
      _loc = Loc.between(currentToken, currentToken),
      _scope = scope,
      _id = ctx.nextMetaId,
      _parentId = _parents.headOption,
      _diagnostics = Array.empty,
      typ = ()
    )

    _parents = meta.id::_parents
    val _start = start.getOrElse(currentToken)
    val node = f(meta, errors)
    val endTok = currentToken.previous.flatMap(_.get).getOrElse(currentToken)
    _parents = _parents.tail
    meta.setDiagnostics(errors)
    meta.setLoc(
      Loc.between(_start, endTok)
    )
    ctx.setParsedNode(meta.id, node)
    node
  }

  def parseDeclaration: Declaration = buildNode((meta, errors) => {

    val modifiersWithTokens = parseModifiers(errors)
    val modifiers = modifiersWithTokens.map(_._2).toSet
    val modifierTokens = modifiersWithTokens.map(_._1)
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
        Declaration.Let(
          meta,
          modifiers,
          pattern,
          rhs
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
        Declaration.Include(
          meta,
          modifiers,
          e
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
        Declaration.TypeAlias(
          meta,
          modifiers,
          ident, kindAnnotation, rhs
        )
      case ENUM =>
        advance()
        val ident = parseIdent()
        withNewScope(enumScope => {
          val genericParams: Array[GenericParam] = currentToken.variant match {
            case LSQB =>
              advance()
              val params = parseGenericParamsList()
              expect(errors)(RSQB)
              params.toArray
            case _ => Array.empty
          }
          expect(errors)(LBRACE)
          val cases = ListBuffer.empty[Declaration.Enum.Case]
          while (!(Set(EOF, RBRACE) contains currentToken.variant)) {
            cases.append(parseEnumCase())
          }
          expect(errors)(RBRACE)
          expect(errors)(SEMICOLON)
          Declaration.Enum(
            meta, modifiers,
            enumScope,
            ident,
            genericParams,
            cases.toArray
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
        errors.appendAll(diagnostics)
        errors.appendAll(skippedDiagnostics)
        Declaration.Error(
          meta,
          modifiers
        )
    }
  })

  private def parseEnumCase(): Declaration.Enum.Case = buildNode((meta, errors) => {
    val name = parseIdent()
    val params: Array[(Option[Ident], TypeAnnotation)] = currentToken.variant match {
      case LPAREN =>
        advance()
        val result = parseCommaSeperatedList(() =>
          peek.variant match {
            case COLON =>
              val ident = parseIdent()
              expect(errors)(COLON)
              (Some(ident), parseTypeAnnotation())
            case _ =>
              (None, parseTypeAnnotation())
          }
        )(Set(ID))
        expect(errors)(RPAREN)
        result.toArray
      case _ =>
        Array.empty
    }
    expect(errors)(SEMICOLON)
    Declaration.Enum.Case(
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

  private def parseKindAnnotation(): KindAnnotation = buildNode((meta, errors) => {
    currentToken.variant match {
      case STAR =>
        val tok = advance()
        KindAnnotation.Star(
          meta
        )
      case LSQB =>
        val startTok = advance()
        val hd = parseKindAnnotation()
        val tl = parseCommaSeperatedListTail(() => parseKindAnnotation())
        expect(errors)(RSQB)
        expect(errors)(FATARROW)
        val to = parseKindAnnotation()
        val params = (hd::tl).toArray
        KindAnnotation.Func(
          meta,
          params, to
        )
      case _ =>
        val loc = currentToken.loc
        val skippedDiagnostics = recover()
        val diagnostics = List(Diagnostic(
          KindExpected,
          Severity.Error,
          loc
        ))
        errors.appendAll(diagnostics)
        errors.appendAll(skippedDiagnostics)

        KindAnnotation.Error(
          meta
        )
    }
  })

  private def parseExpr(): Expression = {
    def parseIntLiteral() = buildNode((meta, errors) => {
      val tok = advance()
      val value = tok.lexeme.replaceAll("_", "").toInt
      Expression.Literal(
        meta,
        Expression.Literal.Int
      )
    })

    def parseFn() = buildNode((meta, errors) => {
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
        Expression.Func(
          meta,
          startTok,
          fnScope,
          genericParams.toArray,
          params.toArray,
          annotation,
          body
        )
      })
    })

    def parseVar() = buildNode((meta, errors) => {
      val ident = parseIdent()
      Expression.Var(
        meta,
        ident
      )
    })

    def parseModule() = buildNode((meta, errors) => {
      val tok = advance()
      withNewScope((moduleScope) => {
        expect(errors)(LBRACE)
        val declarations = {
          val buffer = ListBuffer.empty[Declaration]
          while (Parser.DECL_PREDICTORS.contains(currentToken.variant)) {
            buffer.append(this.parseDeclaration)
          }
          buffer.toArray
        }
        val rbrace = expect(errors)(RBRACE)
        Expression.Module(
          meta,
          moduleScope,
          declarations
        )
      })
    })

    def parseBlock() = buildNode((meta, errors) => withNewScope(blockScope => {
      val members = ListBuffer.empty[Expression.Block.Member]
      val lbrace = advance()
      while (currentToken.variant != RBRACE && currentToken.variant != EOF) {
        val member = if (Parser.DECL_PREDICTORS.contains(currentToken.variant)) {
          parseDeclaration
        } else {
          val e = parseExpr()
          expect(errors)(SEMICOLON)
          e
        }
        members.append(member)
      }
      val rbrace = expect(errors)(RBRACE)

      Expression.Block(
        meta,
        blockScope,
        members.toArray
      )
    }))

    def parseIf() = buildNode((meta, errors) => {
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
      Expression.If(
        meta, predicate, trueBranch, falseBranch
      )
    })

    def parseMatchBranch(errors: ListBuffer[Diagnostic])(): Expression.Match.Branch =
      buildNode((meta, errors) =>
        withNewScope(scope => {
          val p = parsePattern()
          expect(errors)(FATARROW)
          val e = parseExpr()
          Expression.Match.Branch(
            meta,
            scope,
            p,
            e
          )
        })
      )

    def parseMatch() = buildNode((meta, errors) => {
      val matchTok = advance()
      val e = parseExpr()
      expect(errors)(LBRACE)
      val branches = parseCommaSeperatedList(parseMatchBranch(errors))(Parser.PATTERN_PREDICTORS)
      val rbrace = expect(errors)(RBRACE)
      Expression.Match(
        meta,
        e, branches.toArray
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
        buildNode((meta, errors) => {
          val loc = currentToken.loc
          val skippedDiagnostics = recover()
          val diagnostics = List(Diagnostic(
            ExpressionExpected(),
            Severity.Error,
            loc
          ))
          errors.appendAll(diagnostics)
          errors.appendAll(skippedDiagnostics)
          Expression.Error(
            meta
          )
        })
    }
    parseExprTail(head)

  }

  private def parseExprTail(
    head: Expression
  ): Expression = {
    currentToken.variant match {
      case LPAREN =>
        buildNode(start = Some(head), f = (meta, errors) => {
          advance()
          val args = parseCommaSeperatedList(() => parseArg())(Parser.ARG_PREDICTORS).toVector
          expect(errors)(RPAREN)
          Expression.Call(
            meta,
            head, args.toArray
          )
        })

      case DOT =>
        buildNode(start = Some(head), f = (meta,  errors) => {
          advance()
          val prop = parseIdent()
          parseExprTail(
            Expression.Prop(
              meta,
              head,
              prop
            )
          )
        })

      case WITH =>
        buildNode(start = Some(head), f = (meta, errors) => {
          advance()
          val e2 = parseExpr()
          parseExprTail(Expression.With(
            meta,
            head,
            e2
          ))
        })

      case _ => head
    }
  }

  private def parseArg(): Expression.Call.Arg = {
    peek.variant match {
      case EQ =>
        buildNode((meta, _) => {
          val label = parseIdent()
          advance() // eat EQ
          val expr = parseExpr()
          Expression.Call.Arg(meta, Some(label), expr)
        })

      case _ =>
        buildNode((meta, _) => {
          val e = parseExpr()
          Expression.Call.Arg(meta, None, e)
        })

    }
  }

  private def parsePattern(): Pattern = {
    val head = currentToken.variant match {
      case ID =>
        buildNode((meta, _) => {
         val ident = parseIdent()
          Pattern.Var(
            meta,
            ident
          )
        })
      case LPAREN =>
        buildNode((meta, errors) => {
          advance()
          val p = parsePattern()
          expect(errors)(RPAREN)
          Pattern.Paren(meta, p)
        })
      case DOT =>
        buildNode((meta, errors) => {
          val firstTok = advance()
          val ident = parseIdent()
          val lastTok = ident
          Pattern.DotName(
            meta,
            ident
          )
        })
      case _ =>
        buildNode((meta, errors) => {
        val loc = currentToken.loc

          val skippedDiagnostics = recover()
          val diagnostics = List(Diagnostic(
            PatternExpected(),
            Severity.Error,
            loc
          ))
          errors.appendAll(diagnostics)
          errors.appendAll(skippedDiagnostics)
          Pattern.Error(
            meta
          )

        })
    }
    parsePatternTail(head)
  }

  private def parsePatternTail(head: Pattern): Pattern =
      currentToken.variant match {
      case COLON =>
        buildNode(start = Some(head),  f = (meta, errors) => {
          advance()
          val annotation = parseTypeAnnotation()
          Pattern.Annotated(
            meta,
            head,
            annotation
          )
        })
      case LPAREN =>
        buildNode(start = Some(head), f = (meta, errors) => {
          advance()
          val params = parseCommaSeperatedList(parsePatternParam(errors))(
            Parser.PATTERN_PARAM_PREDICTORS
          )
          expect(errors)(RPAREN)
          Pattern.Function(
            meta,
            head,
            params.toArray
          )
        })
      case _ => head
    }

  private def parsePatternParam(errors: ListBuffer[Diagnostic])(): Pattern.Param = {
    currentToken.variant match {
      case DOTDOT =>
        buildNode((meta, errors) => {
          val tok = currentToken
          advance()
          Pattern.Param.Rest(meta)

        })
      case _ =>
        peek.variant match {
          case EQ => buildNode((meta, errors) => {
            val ident = parseIdent()
            expect(errors)(EQ)
            val pattern = parsePattern()
            Pattern.Param.SubPattern(meta, Some(ident), pattern)

          })
          case _ => buildNode((meta, errors) => {
            val p = parsePattern()
            Pattern.Param.SubPattern(meta, None, p)

          })
        }

    }
  }

  private def parseTypeAnnotation(): TypeAnnotation = {
     val head = currentToken.variant match {
       case LPAREN =>
         buildNode((meta, errors) => {
           advance()
           val result = this.parseTypeAnnotation()
           expect(errors)(RPAREN)
           TypeAnnotation.Paren(meta, result)
         })

       case LSQB => buildNode((meta, errors) => {

         withNewScope(scope => {
           advance()
           val genericParams = parseGenericParamsList()
           expect(errors)(RSQB)
           expect(errors)(FATARROW)
           val annotation = parseTypeAnnotation()
           TypeAnnotation.Forall(
             meta,
             scope, genericParams.toArray, annotation
           )
         })
       })
       case FN => buildNode((meta, errors) => {

         val startTok = advance()
         expect(errors)(LPAREN)
         val params = parseCommaSeperatedList(() => {
           val label = peek.variant match {
             case COLON =>
               val ident = parseIdent()
               advance() // consume colon
               Some(ident)
             case _ =>
               None
           }
           val typ = parseTypeAnnotation()
           (label, typ)
         })(Parser.TYPE_PREDICTORS)
         expect(errors)(RPAREN)
         expect(errors)(FATARROW)
         val returnType = parseTypeAnnotation()
         TypeAnnotation.Func(meta, params.toArray, returnType)
       })
       case v if Parser.EXPR_PREDICTORS.contains(v) =>
         val e = parseExpr()
         e match {
           case Expression.Var(varMeta, ident) =>
             TypeAnnotation.Var(varMeta, ident)
           case Expression.Prop(eMeta, e, prop) =>
             TypeAnnotation.Prop(
               eMeta, e, prop
             )
           case _ =>
             TypeAnnotation.Error(
               e.meta.withDiagnostic(
                 Diagnostic(
                   variant = TypeExpected(),
                   severity = Severity.Error,
                   loc = e.loc
                 )
               )
             )
         }
       case _ =>
         buildNode((meta, errors) => {
           val loc = currentToken.loc
           val skippedDiagnostics = recover()
           val diagnostics = List(Diagnostic(
              TypeExpected(),
              Severity.Error,
              loc
           ))
           errors.appendAll(diagnostics)
           errors.appendAll(skippedDiagnostics)
           TypeAnnotation.Error(
             meta
           )
         })

     }
    parseTypeAnnotationTail(head)
  }

  private def parseTypeAnnotationTail(head: TypeAnnotation): TypeAnnotation =
      currentToken.variant match {
        case LSQB =>
          buildNode(start = Some(head), f = (meta,  errors) => {
            advance()
            val hd = parseTypeAnnotation()
            val tl = parseCommaSeperatedListTail(() => parseTypeAnnotation())
            val args = hd :: tl
            val rsqb = expect(errors)(RSQB)
            parseTypeAnnotationTail(
              TypeAnnotation.TApplication(
                meta,
                head, args.toArray
              )
            )
          })
        case _ => head
      }

  private def parseParam(): Expression.Param = {
    val pattern = parsePattern()
    Expression.Param(pattern)
  }

  private def parseGenericParamsList(): Iterable[GenericParam] = {
    val hd = parseGenericParam()
    val tl = parseCommaSeperatedListTail(() => parseGenericParam())
    hd::tl
  }

  private def parseGenericParam(): GenericParam = buildNode((meta, errors) => {
    val ident = parseIdent()
    val kindAnnotation = currentToken.variant match {
      case COLON =>
        advance()
        Some(parseKindAnnotation())
      case _ => None
    }
    GenericParam(meta, ident, kindAnnotation)
  })

  private def parseIdent(): Ident = buildNode((meta, errors) =>{
    val tok = expect(errors)(ID)
    Ident(
      meta,
      tok.lexeme
    )
  })

  private def parseCommaSeperatedList[T](
    f: () => T)(predictors: Set[token.Variant]): List[T] = {
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