package wacc

import parsley.Parsley.attempt
import parsley.combinator.{many, sepBy}
import parsley.errors.combinator.ErrorMethods
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
    attempt(arrayElem) <|>
    IdentExpr(IDENT) <|>
    IfExpr(IF ~> expr, THEN ~> expr, ELSE ~> expr <~ FI).label("If expression") <|>
    (PairExpr <# PAIR_LITER)).label("Atomic Literal")
    .explain("--> Atomic Literals includes booleans, chars, strings, " +
      "array-elems `identifier[]` or identifiers")

  lazy val expr: Parsley[Expr] =
    precedence(atomExpr, OPENPAREN ~> expr <~ CLOSEDPAREN)(
      Ops(Prefix)(NotExpr <# "!".label("unary_op"), NegExpr <# NEGATE.label("unary_op"),
        LenExpr <# LEN.label("unary_op"), OrdExpr <# ORD.label("unary_op"),
        ChrExpr <# CHR.label("unary_op")),
      Ops(InfixL)(ModExpr <# ("%").label("binary_op"),
        DivExpr <# ("/").label("binary_op"), MulExpr <# "*".label("binary_op")),
      Ops(InfixL)(AddExpr <# ("+").label("binary_op"),
        SubExpr <# ("-").label("binary_op")),
      Ops(InfixL)(attempt(GTEQExpr <# (">=").label("comparison_op")),
        GTExpr <# (">").label("comparison_op"),
        attempt(LTEQExpr <# ("<=").label("comparison_op")),
        LTExpr <# ("<").label("comparison_op")),
      Ops(InfixL)(EQExpr <# ("==").label("comparison_op"),
        NEQExpr <# ("!=").label("comparison_op")),
      Ops(InfixL)(AndExpr <# ("&&").label("logical_op")),
      Ops(InfixL)(OrExpr <# ("||").label("logical_op"))
    ).label("expression")
      .explain("--> Expressions are atomic literals preceded by or followed by" +
        " unary, binary, logical and comparison operators")

  // Lvalue parsers
  lazy val lvalue: Parsley[LValue] = attempt(arrayElem) <|> IdentValue(IDENT) <|> pairElem
  lazy val pairElem: Parsley[PairElem] = (FST ~> Fst(lvalue)) <|> (SND ~> Snd(lvalue))
  lazy val arrayElem: Parsley[ArrayElem] = ArrayElem(IDENT, some(OPENSQUAREBRAC ~> expr <~ CLOSESQUAREBRAC))

  // Rvalue parsers
  lazy val rvalue: Parsley[RValue] = call <|> expr <|> pairElem <|> newPair <|> arrayLiter
  lazy val expList: Parsley[List[Expr]] = sepBy(expr, ",")
  // type hierarchy parsers
  lazy val waccType: Parsley[Type] = attempt(arrayType) <|> baseType <|> pairType
  lazy val arrayType: Parsley[ArrayType] = chain.postfix1((baseType <|> pairType), (OPENSQUAREBRAC ~> CLOSESQUAREBRAC) #> ArrayType)
  lazy val pairElemType: Parsley[PairElemType] = attempt(arrayType) <|> baseType <|> attempt(waccType) <|> (DummyPair <# PAIR) // here attempt something - pair(wacc, wacc)
  // terminal statements
  lazy val terminalStat: Parsley[Statement] = (returnStat <|> exit).label("return/exit_statement")
  lazy val statement: Parsley[Statement]
  = chain.left1[Statement](statAtoms <|> terminalStat, SEMICOLON #> ConsecStat)
  // highest level parsers
  lazy val param: Parsley[Param] = Param(waccType, IDENT)
  lazy val paramList: Parsley[List[Param]] = sepBy(param, ",")
  val call: Parsley[RValue] = Call((CALL ~> IDENT <~ OPENPAREN), expList <~ CLOSEDPAREN)
  val newPair: Parsley[RValue] = NewPair((NEWPAIR ~> OPENPAREN ~> expr), COMMA ~> expr <~ CLOSEDPAREN)
  val arrayLiter: Parsley[RValue] = ArrayLiter(OPENSQUAREBRAC ~> expList <~ CLOSESQUAREBRAC)
  val baseType: Parsley[BaseType] = ((IntType <# "int") <|>
    (BoolType <# "bool") <|>
    (CharType <# "char") <|>
    (StringType <# "string")).label("primitive_base_type")
  val pairType: Parsley[Type] = PairType(PAIR ~> OPENPAREN ~> pairElemType, COMMA ~> pairElemType <~ CLOSEDPAREN)
  // Statement Parsers
  val skip: Parsley[Statement] = Skip <# "skip".label("Statement_beginning")
  val varDec: Parsley[Statement] = VarDec(waccType, IDENT, ("=" ~> rvalue))
  val assign: Parsley[Statement] = Assign(lvalue, ("=" ~> rvalue))
  val read: Parsley[Statement] = Read(READ.label("Statement_beginning") ~> lvalue)
  val free: Parsley[Statement] = Free(FREE.label("Statement_beginning") ~> expr)
  val print: Parsley[Statement] = Print(PRINT.label("Statement_beginning") ~> expr)
  val println: Parsley[Statement] = Println(PRINTLN.label("Statement_beginning") ~> expr)
  val ifStat: Parsley[Statement] = If((IF.label("Statement_beginning") ~> expr), (THEN ~> statement), (ELSE ~> statement <~ FI))
  val ifThenStat: Parsley[Statement] = IfThen((IF.label("Statement_beginning") ~> expr), (THEN ~> statement <~ FI))
  val whileStat: Parsley[Statement] = While((WHILE.label("Statement_beginning") ~> expr), (DO ~> statement <~ DONE))
  val scopeStat: Parsley[Statement] = ScopeStat(BEGIN.label("Statement_beginning") ~> statement <~ END)
  val matchStat: Parsley[Statement] = MatchStat(MATCH ~> expr <~ COLON , some(OPENCURLY ~> CASE ~> expr <~ ARROW <~> statement <~ CLOSEDCURLY))
  val returnStat: Parsley[Statement] = Return(RETURN ~> expr)
  val exit: Parsley[Statement] = Exit(EXIT ~> expr)

  val statAtoms: Parsley[Statement] = skip <|> varDec <|> assign <|> read <|>
    free <|> attempt(println) <|> print <|> attempt(ifStat) <|> ifThenStat <|>
    whileStat <|> scopeStat <|> matchStat
  val func: Parsley[Func]
  = Func(attempt(waccType <~> IDENT <~ OPENPAREN), paramList <~ CLOSEDPAREN,
    IS ~> statement.filter(isValidFuncStatement).explain("Function body starting here must" +
      " have a return/exit statement on all paths and must end with one") <~ END)

  val program: Parsley[Program]
  = Program(BEGIN ~> many(func), statement <~ END)

  private def isValidFuncStatement(stat: Statement): Boolean = {
    stat match {
      case ConsecStat(_, next) =>
        isValidFuncStatement(next)
      case If(_, thenStat, elseStat) =>
        isValidFuncStatement(thenStat) && isValidFuncStatement(elseStat)
      case IfThen(_, thenStat) => false
      case While(_, doStat) => isValidFuncStatement(doStat)
      case ScopeStat(stat) => isValidFuncStatement(stat)
      case Exit(_) => true
      case Return(_) => true
      case _ => false
    }
  }

}
