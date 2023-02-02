package wacc

import parsley.Parsley.attempt
import parsley.combinator.{many, sepBy}
import parsley.errors.combinator.{ErrorMethods, amend, fail}
import parsley.expr.{Prefix, chain, precedence}

object parser {

  import AST._
  import lexer._
  import lexer.implicits.implicitSymbol
  import parsley.Parsley
  import parsley.combinator.some
  import parsley.expr.{InfixL, Ops}


  // Expr parsers
  lazy val atomExpr: Parsley[Expr] = (IntExpr(INT) <|>
    BoolExpr(BOOL) <|>
    CharExpr(CHAR) <|>
    StringExpr(STRING) <|>
    attempt(arrayelem) <|>
    IdentExpr(IDENT) <|>
    (PAIR_LITER #> PairExpr)).label("Atomic Literal")
    .explain("--> Atomic Literals includes booleans, chars, strings, " +
      "array-elems `identifier[]` or identifiers")

  lazy val expr: Parsley[Expr] =
    precedence(atomExpr, OPENPAREN ~> expr <~ CLOSEDPAREN)(
      Ops(Prefix)("!".label("unary op") #> NotExpr, "-".label("unary op") #> NegExpr,
        "len".label("unary op") #> LenExpr, "ord".label("unary op") #> OrdExpr,
        "chr".label("unary op") #> ChrExpr),
      Ops(InfixL)(("%").label("binary op") #> ModExpr,
        ("/").label("binary op") #> DivExpr, ("*").label("binary op") #> MulExpr),
      Ops(InfixL)(("+").label("binary op") #> AddExpr,
        ("-").label("binary op") #> SubExpr),
      Ops(InfixL)(attempt((">=").label("comparison op") #> GTEQExpr),
        (">").label("comparison op") #> GTExpr,
        attempt(("<=").label("comparison op") #> LTEQExpr),
        ("<").label("comparison op") #> LTExpr),
      Ops(InfixL)(("==").label("comparison op") #> EQExpr,
        ("!=").label("comparison op") #> NEQExpr),
      Ops(InfixL)(("&&").label("logical op") #> AndExpr),
      Ops(InfixL)(("||").label("logical op") #> OrExpr)
    ).label("expression")
      .explain("--> Expressions are atomic literals preceded by or followed by" +
        " unary, binary, logical and comparison operators")

  // Lvalue parsers
  lazy val lvalue: Parsley[LValue] = attempt(arrayelem) <|> IdentValue(IDENT) <|> pairelem
  lazy val pairelem: Parsley[PairElem] = (FST ~> Fst(lvalue)) <|> (SND ~> Snd(lvalue))
  lazy val arrayelem: Parsley[ArrayElem] = ArrayElem(IDENT, some(OPENSQUAREBRAC ~> expr <~ CLOSESQUAREBRAC))

  // Rvalue parsers
  lazy val rvalue: Parsley[RValue] = call <|> expr <|> pairelem <|> newPair <|> arrayLiter
  val call: Parsley[RValue] = Call((CALL ~> IDENT <~ OPENPAREN), expList <~ CLOSEDPAREN)
  val newPair: Parsley[RValue] = NewPair((NEWPAIR ~> OPENPAREN ~> expr), COMMA ~> expr <~ CLOSEDPAREN)
  lazy val expList: Parsley[List[Expr]] = sepBy(expr, ",")
  val arrayLiter: Parsley[RValue] = ArrayLiter(OPENSQUAREBRAC ~> expList <~ CLOSESQUAREBRAC)

  // type heirarchy parsers
  lazy val waccType: Parsley[Type] = attempt(arrayType) <|> baseType <|> pairType
  val baseType: Parsley[BaseType] = (("int" #> IntType) <|>
    ("bool" #> BoolType) <|>
    ("char" #> CharType) <|>
    ("string" #> StringType)).label("primitive base type")
  lazy val arrayType: Parsley[ArrayType] = chain.postfix1((baseType <|> pairType), (OPENSQUAREBRAC ~> CLOSESQUAREBRAC) #> ArrayType)
  val pairType: Parsley[Type] = PairType(PAIR ~> OPENPAREN ~> pairelemType, COMMA ~> pairelemType <~ CLOSEDPAREN)
  lazy val pairelemType: Parsley[PairElemType] = attempt(arrayType) <|> baseType <|> (PAIR #> DummyPair)

  // Statement Parsers
  val skip: Parsley[Statement] = "skip".label("Statement beginning") #> Skip
  val assigneq: Parsley[Statement] = AssignEq(waccType, IDENT, ("=" ~> rvalue))
  val equals: Parsley[Statement] = Equals(lvalue, ("=" ~> rvalue))
  val read: Parsley[Statement] = Read(READ.label("Statement beginning") ~> lvalue)
  val free: Parsley[Statement] = Free(FREE.label("Statement beginning") ~> expr)
  val print: Parsley[Statement] = Print(PRINT.label("Statement beginning") ~> expr)
  val println: Parsley[Statement] = Println(PRINTLN.label("Statement beginning") ~> expr)
  val ifStat: Parsley[Statement] = If((IF.label("Statement beginning") ~> expr), (THEN ~> statement), (ELSE ~> statement <~ FI))
  val whileStat: Parsley[Statement] = While((WHILE.label("Statement beginning") ~> expr), (DO ~> statement <~ DONE))
  val scopeStat: Parsley[Statement] = ScopeStat(BEGIN.label("Statement beginning") ~> statement <~ END)

  // terminal statements
  lazy val terminalStat: Parsley[Statement] = (returnStat <|> exit).label("return/exit statement")
  val returnStat: Parsley[Statement] = Return(RETURN ~> expr)
  val exit: Parsley[Statement] = Exit(EXIT ~> expr)

  val statAtoms: Parsley[Statement] = skip <|> assigneq <|> equals <|> read <|>
    free <|> attempt(println) <|> print <|>
    ifStat <|> whileStat <|> scopeStat

  lazy val statement: Parsley[Statement]
  = chain.left1[Statement](statAtoms <|> terminalStat, SEMICOLON #> ConsecStat)

  // highest level parsers
  lazy val param: Parsley[Param] = Param(waccType, IDENT)
  lazy val paramList: Parsley[List[Param]] = sepBy(param, ",")

  val func: Parsley[Func]
  = Func(waccType,
    IDENT,
    OPENPAREN ~> paramList <~ CLOSEDPAREN,
    (IS ~> statement.filter(isValidFuncStatement) <~ END))

  val func_body: Parsley[Statement]
  = amend {
    attempt(waccType ~> IDENT ~> OPENPAREN
      ~> fail("Function starting here must have a return/exit statement on all paths " +
      "and also end with one"))
  } <|>
    statement

  def isValidFuncStatement(stat: Statement): Boolean = {
    stat match {
      case ConsecStat(first, next) =>
        val found2 = isValidFuncStatement(next)
        if (!found2) {
          //          isValidFuncStatement(first)
          false
        } else true
      case If(_, thenStat, elseStat) =>
        isValidFuncStatement(thenStat) && isValidFuncStatement(elseStat)
      case While(_, doStat) => isValidFuncStatement(doStat)
      case ScopeStat(stat) => isValidFuncStatement(stat)
      case Exit(e) => true
      case Return(e) => true
      case _ => false
    }
  }

  val program: Parsley[Program] = Program(BEGIN ~> many(attempt(func)), ((func_body) <~ END))
}

