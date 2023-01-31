package wacc

import parsley.Parsley.attempt
import parsley.combinator.{many, sepBy}
import parsley.expr.{Prefix, chain, precedence}

object parser {

  import AST._
  import lexer._
  import lexer.implicits.implicitSymbol
  import parsley.Parsley
  import parsley.combinator.some
  import parsley.expr.{InfixL, Ops}

  // Expr parsers
  lazy val atomExpr: Parsley[Expr] = IntExpr(INT) <|>
    BoolExpr(BOOL) <|>
    CharExpr(CHAR) <|>
    StringExpr(STRING) <|>
    attempt(arrayelem) <|>
    IdentExpr(IDENT) <|>
    (PAIR_LITER #> PairExpr)

  lazy val expr: Parsley[Expr] =
    precedence(atomExpr, OPENPAREN ~> expr <~ CLOSEDPAREN)(
      Ops(Prefix)("!" #> NotExpr, "-" #> NegExpr, "len" #> LenExpr, "ord" #> OrdExpr,
        ("chr") #> ChrExpr),
      Ops(InfixL)(("%") #> ModExpr, ("/") #> DivExpr, ("*") #> MulExpr),
      Ops(InfixL)(("+") #> AddExpr, ("-") #> SubExpr),
      Ops(InfixL)(attempt((">=") #> GTEQExpr), (">") #> GTExpr, attempt(("<=") #> LTEQExpr), ("<") #> LTExpr),
      Ops(InfixL)(("==") #> EQExpr, ("!=") #> NEQExpr),
      Ops(InfixL)(("&&") #> AndExpr),
      Ops(InfixL)(("||") #> OrExpr)
    )

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
  val baseType: Parsley[BaseType] = ("int" #> IntType) <|>
    ("bool" #> BoolType) <|>
    ("char" #> CharType) <|>
    ("string" #> StringType)
  lazy val arrayType: Parsley[ArrayType] = chain.postfix1((baseType <|> pairType), (OPENSQUAREBRAC ~> CLOSESQUAREBRAC) #> ArrayType)
  val pairType: Parsley[Type] = PairType(PAIR ~> OPENPAREN ~> pairelemType, COMMA ~> pairelemType <~ CLOSEDPAREN)
  lazy val pairelemType: Parsley[PairElemType] = attempt(arrayType) <|> baseType <|> (PAIR #> DummyPair)

  // Statement Parsers
  val skip: Parsley[Statement] = "skip" #> Skip
  val assigneq: Parsley[Statement] = AssignEq(waccType, IDENT, ("=" ~> rvalue))
  val equals: Parsley[Statement] = Equals(lvalue, ("=" ~> rvalue))
  val read: Parsley[Statement] = Read(READ ~> lvalue)
  val free: Parsley[Statement] = Free(FREE ~> expr)
  val returnStat: Parsley[Statement] = Return(RETURN ~> expr)
  val exit: Parsley[Statement] = Exit(EXIT ~> expr)
  val print: Parsley[Statement] = Print(PRINT ~> expr)
  val println: Parsley[Statement] = Println(PRINTLN ~> expr)
  val ifStat: Parsley[Statement] = If((IF ~> expr), (THEN ~> statement), (ELSE ~> statement <~ FI))
  val whileStat: Parsley[Statement] = While((WHILE ~> expr), (DO ~> statement <~ DONE))
  val scopeStat: Parsley[Statement] = ScopeStat(BEGIN ~> statement <~ END)

  val statAtoms: Parsley[Statement] = skip <|> assigneq <|> equals <|> read <|>
    free <|> returnStat <|> exit <|> attempt(println) <|> print <|>
    ifStat <|> whileStat <|> scopeStat

  lazy val statement: Parsley[Statement]
  = chain.left1[Statement](statAtoms, SEMICOLON #> ConsecStat)

  // highest level parsers
  lazy val param: Parsley[Param] = Param(waccType, IDENT)
  lazy val paramList: Parsley[List[Param]] = sepBy(param, ",")

  val func: Parsley[Func]
  = Func(waccType, IDENT, OPENPAREN ~> paramList <~ CLOSEDPAREN, (IS ~> statement <~ END))


  val program: Parsley[Program] = Program(BEGIN ~> many(attempt(func)), (statement <~ END))


}

