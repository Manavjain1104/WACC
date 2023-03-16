package wacc

import parsley.combinator.{ifP, many, sepBy, sepBy1}
import parsley.Parsley.{attempt, notFollowedBy, unit, lookAhead}
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
   lazy val atomExpr: Parsley[Expr] = (
    IntExpr(INT) <|>
      BoolExpr(BOOL) <|>
      CharExpr(CHAR) <|>
      StringExpr(STRING) <|>
      attempt(arrayelem) <|>
      attempt(structelem) <|>
      attempt(classelem) <|>
      IfExpr(IF ~> expr, THEN ~> expr, ELSE ~> expr <~ FI).label("If expression") <|>
      ThisExpr(THIS ~> DOT ~> IDENT) <* notFollowedBy(OPENPAREN).explain("`this` can only be applied to non methods") <|>
      IdentExpr(IDENT) <|>
      (PairExpr <# PAIR_LITER)).label("Atomic Literal")
    .explain("--> Atomic Literals includes booleans, chars, strings, " + "array-elems, struct-elems or `identifier[]` or identifiers")

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
  lazy val lvalue: Parsley[LValue] = attempt(arrayelem) <|> IdentValue(IDENT) <|> pairelem
  lazy val pairelem: Parsley[PairElem] = (FST ~> Fst(lvalue)) <|> (SND ~> Snd(lvalue))
  lazy val arrayelem: Parsley[ArrayElem] = ArrayElem(IDENT, some(OPENSQUAREBRAC ~> expr <~ CLOSESQUAREBRAC))
  lazy val structelem: Parsley[StructElem] = StructElem(IDENT <~ ARROW, IDENT)
  lazy val classelem: Parsley[ClassElem] = ClassElem(IDENT <~ DOT, IDENT)

  // Rvalue parsers
  lazy val rvalue: Parsley[RValue] = call <|> attempt(methodCall) <|> expr <|> pairelem <|> newPair <|> newClass <|> arrayLiter <|> structLiter
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
  val newClass: Parsley[RValue] = NewClass(NEW ~> IDENT, OPENPAREN ~> expList <~ CLOSEDPAREN)
  val newPair: Parsley[RValue] = NewPair((NEWPAIR ~> OPENPAREN ~> expr), COMMA ~> expr <~ CLOSEDPAREN)
  val arrayLiter: Parsley[RValue] = ArrayLiter(OPENSQUAREBRAC ~> expList <~ CLOSESQUAREBRAC)
 val structLiter: Parsley[RValue] = StructLiter(OPENCURLY ~> expList <~ CLOSEDCURLY)
  val methodCall: Parsley[RValue] = MethodCall(IDENT <~ DOT, IDENT <~ OPENPAREN, expList <~ CLOSEDPAREN)
  lazy val expList: Parsley[List[Expr]] = sepBy(expr, ",")

  // type hierarchy parsers
  lazy val waccType: Parsley[Type] = attempt(arrayType) <|> baseType <|> pairType <|> structType <|> classType
  val structType: Parsley[StructType] = StructType(STRUCT ~> IDENT)
  val voidType: Parsley[VoidType] = VoidType <# symbol("void")
  val classType: Parsley[ClassType] = ClassType(CLASS ~> IDENT)
  val baseType: Parsley[BaseType] =
    ((IntType <# "int") <|>
      (BoolType <# "bool") <|>
      (CharType <# "char") <|>
      (StringType <# "string")).label("primitive_base_type")
  lazy val arrayType: Parsley[ArrayType] = chain.postfix1((baseType <|> pairType <|> classType <|> structType), (OPENSQUAREBRAC ~> CLOSESQUAREBRAC) #> ArrayType)
  val pairType: Parsley[Type] = PairType(PAIR ~> OPENPAREN ~> pairelemType, COMMA ~> pairelemType <~ CLOSEDPAREN)
  lazy val pairelemType: Parsley[PairElemType] = attempt(arrayType) <|> baseType <|> attempt(waccType) <|> (DummyPair <# PAIR) // here attempt something - pair(wacc, wacc)

  // Statement Parsers
  val skip: Parsley[Statement] = Skip <# "skip".label("Statement_beginning")
  val vardec: Parsley[Statement] = VarDec(waccType, IDENT, ("=" ~> rvalue))
  val fieldDec: Parsley[FieldDec] = FieldDec(waccType, IDENT)
  val assign: Parsley[Statement] = Assign(lvalue, ("=" ~> rvalue))
  val read: Parsley[Statement] = Read(READ.label("Statement_beginning") ~> lvalue)
  val free: Parsley[Statement] = Free(FREE.label("Statement_beginning") ~> expr)
  val print: Parsley[Statement] = Print(PRINT.label("Statement_beginning") ~> expr)
  val println: Parsley[Statement] = Println(PRINTLN.label("Statement_beginning") ~> expr)
  val ifStat: Parsley[Statement] = If((IF.label("Statement_beginning") ~> expr), (THEN ~> statement), (ELSE ~> statement <~ FI))
  val ifThenStat: Parsley[Statement] = IfThen((IF.label("Statement_beginning") ~> expr), (THEN ~> statement <~ FI))
  val whileStat: Parsley[Statement] = While((WHILE.label("Statement_beginning") ~> expr), (DO ~> statement <~ DONE))
  val scopeStat: Parsley[Statement] = ScopeStat(BEGIN.label("Statement_beginning") ~> statement <~ END)
  val callStat: Parsley[Statement] = CallStat((CALL ~> IDENT <~ OPENPAREN), expList <~ CLOSEDPAREN)
  val methodStat: Parsley[Statement] = MethodStat(IDENT <~ DOT, IDENT <~ OPENPAREN, expList <~ CLOSEDPAREN)
  val matchStat: Parsley[Statement] = MatchStat(MATCH ~> expr <~ COLON , some(OPENCURLY ~> CASE ~> expr <~ ARROW <~> statement <~ CLOSEDCURLY))
  // terminal statements
  lazy val terminalStat: Parsley[Statement] = (returnStat <|> exit).label("return/exit_statement")
  val returnStat: Parsley[Statement] = Return(RETURN ~> expr)
  val exit: Parsley[Statement] = Exit(EXIT ~> expr)

  val statAtoms: Parsley[Statement] = skip <|> attempt(callStat) <|> vardec <|> attempt(methodStat) <|> assign <|> read <|>
    free <|> attempt(println) <|> print <|> <|> attempt(ifStat) <|> ifThenStat <|>
    whileStat <|> scopeStat <|> matchStat

  lazy val statement: Parsley[Statement]
  = chain.left1[Statement](statAtoms <|> terminalStat, SEMICOLON #> ConsecStat)


  // highest level parsers
  lazy val param: Parsley[Param] = Param(waccType, IDENT)
  lazy val paramList: Parsley[List[Param]] = sepBy(param, ",")

  val func: Parsley[Func]
  = Func(attempt(waccType <~> IDENT <~ OPENPAREN), paramList <~ CLOSEDPAREN,
    IS ~> statement.filter(isValidFuncStatement).explain("Function body starting here must" +
      " have a return/exit statement on all paths and must end with one") <~ END)

  val func: Parsley[Func] = ifP(lookAhead(voidType) #> true <|> lookAhead(waccType) #> false, voidFunc, regFunc)

  val struct: Parsley[Struct] = Struct(attempt(STRUCT ~> IDENT <~ OPENCURLY),
    (sepBy1(fieldDec, SEMICOLON).explain("Expected non empty struct field variable declaration")
      <~ CLOSEDCURLY).explain("Invalid Struct Definition. Check Syntax."))

  val scope: Parsley[Scope] = (Private <# PRIVATE) <|> (Public <# PUBLIC) <|> (Public <# unit)
  val classField: Parsley[ClassField] = ClassField(scope, vardec)

  val method: Parsley[Method] = Method(scope, func)

  val waccClass: Parsley[Class]
  = Class(attempt(CLASS ~> IDENT <~ OPENPAREN),
    paramList <~ CLOSEDPAREN <~ OPENCURLY,
    sepBy(attempt(classField), SEMICOLON),
    many(method) <~ CLOSEDCURLY)

  val program: Parsley[Program]
  = Program(BEGIN ~> many(waccClass), many(struct), many(func), statement <~ END)

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

  private def isValidVoidFuncStatement(stat: Statement): Boolean = {
    stat match {
      case ConsecStat(first, next) =>
        isValidVoidFuncStatement(next) && isValidVoidFuncStatement(first)
      case If(_, thenStat, elseStat) =>
        isValidVoidFuncStatement(thenStat) && isValidVoidFuncStatement(elseStat)
      case While(_, doStat) => isValidFuncStatement(doStat)
      case ScopeStat(stat) => isValidFuncStatement(stat)
      case _: Return => false
      case _ => true
    }
  }


}

