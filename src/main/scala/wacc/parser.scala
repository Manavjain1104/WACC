package wacc

import parsley.expr.{Prefix, precedence}

object parser {

  import AST._
  import lexer._
  import parsley.Parsley
  import parsley.expr.{InfixL, Ops}
  import parsley.implicits.character.charLift

  lazy val atomExpr: Parsley[Expr] = IntExpr(INT) <|>
    BoolExpr(BOOL) <|>
    CharExpr(CHAR) <|>
    StringExpr(STRING) <|>
    IndentExpr(IDENT) <|>
    (PAIR_LITER #> PairExpr)

  lazy val expr: Parsley[Expr] =
    precedence(atomExpr, '(' ~> expr <~ ')')(
      Ops(Prefix)('!' #> NotExpr, '-' #> NegExpr, symbol("len") #> LenExpr, symbol("ord") #> OrdExpr,
        symbol("chr") #> ChrExpr),
      Ops(InfixL)('%' #> ModExpr, '/' #> DivExpr, '*' #> MulExpr),
      Ops(InfixL)('+' #> AddExpr, '-' #> SubExpr),
      Ops(InfixL)('>' #> GTExpr, symbol(">=") #> GTEQExpr, '<' #> LTExpr, symbol("<=") #> LTEQExpr),
      Ops(InfixL)(symbol("==") #> EQExpr, symbol("!=") #> NEQExpr),
      Ops(InfixL)(symbol("&&") #> AndExpr),
      Ops(InfixL)(symbol("||") #> OrExpr)
    )

}

