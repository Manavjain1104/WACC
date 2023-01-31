package wacc

import parsley.Parsley.attempt
import parsley.combinator.{exactly, sepBy}
import parsley.expr.{Prefix, precedence}

object parser {

  import AST._
  import lexer._
  import parsley.Parsley
  import parsley.combinator.some
  import parsley.expr.{InfixL, Ops}
  import parsley.implicits.character.charLift

  // Expr parsers
  lazy val atomExpr: Parsley[Expr] = IntExpr(INT) <|>
    BoolExpr(BOOL) <|>
    CharExpr(CHAR) <|>
    StringExpr(STRING) <|>
    IndentExpr(IDENT) <|>
    arrayelem <|>
    (PAIR_LITER #> PairExpr)

  lazy val expr: Parsley[Expr] =
    precedence(atomExpr, OPENPAREN ~> expr <~ CLOSEDPAREN)(
      Ops(Prefix)(symbol("!") #> NotExpr, symbol("-") #> NegExpr, symbol("len") #> LenExpr, symbol("ord") #> OrdExpr,
        symbol("chr") #> ChrExpr),
      Ops(InfixL)(symbol("%") #> ModExpr, symbol("/") #> DivExpr, symbol("*") #> MulExpr),
      Ops(InfixL)(symbol("+") #> AddExpr, symbol("-") #> SubExpr),
      Ops(InfixL)(attempt(symbol(">=") #> GTEQExpr), symbol(">") #> GTExpr, attempt(symbol("<=") #> LTEQExpr), symbol("<") #> LTExpr),
      Ops(InfixL)(symbol("==") #> EQExpr, symbol("!=") #> NEQExpr),
      Ops(InfixL)(symbol("&&") #> AndExpr),
      Ops(InfixL)(symbol("||") #> OrExpr)
    )

  // Lvalue parsers
  lazy val lvalue: Parsley[LValue] = attempt(arrayelem) <|> IdentValue(IDENT) <|> pairelem
  lazy val pairelem: Parsley[PairElem] = (FST ~> Fst(lvalue)) <|> (SND ~> Snd(lvalue))
  lazy val arrayelem: Parsley[ArrayElem] = ArrayElem(IDENT, some(OPENSQUAREBRAC ~> expr <~ CLOSESQUAREBRAC))

  // Rvalue parsers
  lazy val rvalue: Parsley[RValue] = call <|> expr <|> pairelem <|> newPair <|> arrayLiter
  val call: Parsley[RValue] = Call((CALL ~> IDENT <~ OPENPAREN), (expList <~ CLOSEDPAREN))
  val newPair: Parsley[RValue] = NewPair((NEWPAIR ~> OPENPAREN ~> expr), (COMMA ~> expr <~ CLOSEDPAREN))
  lazy val expList: Parsley[List[Expr]] = sepBy(expr, symbol(","))
  val arrayLiter: Parsley[RValue] = ArrayLiter(OPENSQUAREBRAC ~> expList <~ CLOSESQUAREBRAC)

  // type heirarchy parsers
  lazy val waccType : Parsley[Type] = attempt(arrayType) <|> baseType <|> pairType
  val baseType: Parsley[BaseType] = (symbol("int") #> IntType) <|>
    (symbol("bool") #> BoolType) <|>
    (symbol("char") #> CharType) <|>
    (symbol("string") #> StringType)
  lazy val arrayType : Parsley[ArrayType] = ArrayType(~waccType <~ OPENSQUAREBRAC <~ CLOSESQUAREBRAC)
  val pairType: Parsley[Type] = PairType((PAIR ~> OPENPAREN ~> pairelemType), (COMMA ~> pairelemType <~ CLOSEDPAREN))
  lazy val pairelemType : Parsley[PairElemType] = baseType <|> (PAIR #> DummyPair) <|> arrayType



  /*
  * What and how do we test - show what Krkn have been doing
  * Recursive type problem = laziness
  * Invalid bracketing doesnt take care
  * Difference between String and Ident
  * */

  // Statement Parsers

}

