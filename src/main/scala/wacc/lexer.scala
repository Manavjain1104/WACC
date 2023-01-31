package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.NameDesc
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.symbol.ImplicitSymbol

object lexer {

  import parsley.token.{descriptions, predicate}
  import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc}

  def identStart(c: Char): Boolean = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  def identContinue(c: Char): Boolean = (c >= '0' && c <= '9') || identStart(c)

  def graphicChar(c: Char): Boolean = ((c >= ' ') && (!Set('\\', '\'', '\"').contains(c))) || escapes.contains(c)

  val escapes = Set('\\', '\u0000', '\b', '\t', '\n', '\f', '\r', '\"', '\'')

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(identStart), // ask Jamie about Basic
      identifierLetter = predicate.Basic(identContinue)
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      commentLineAllowsEOF = true,
      space = predicate.Basic(Character.isWhitespace)
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set[String]("begin", "end", "is", "skip", "free", "read",
        "return", "exit", "print", "println", "if", "then", "else", "fi", "while", "do", "done",
        "snd", "fst", "newpair", "call", "int", "bool", "char", "string", "pair", "true", "false",
        "null"
      ),
      hardOperators = Set[String]("!", "-", "len", "ord", "chr", "*", "/", "%", "+", ">", ">=",
        "<", "<=", "==", "!=", "&&", "||"),
      caseSensitive = true
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = escapes,
      ),
      graphicCharacter = predicate.Basic(graphicChar)
    ),
  )

  private val lexer = new Lexer(desc)

  // token definitions
  val BEGIN: Parsley[Unit] = lexer.lexeme.symbol("begin")
  val END: Parsley[Unit] = lexer.lexeme.symbol("end")
  val OPENPAREN: Parsley[Unit] = lexer.lexeme.symbol.openParen
  val CLOSEDPAREN: Parsley[Unit] = lexer.lexeme.symbol.closingParen
  val IS: Parsley[Unit] = lexer.lexeme.symbol("is")
  val COMMA: Parsley[Unit] = lexer.lexeme.symbol(",")
  val SKIP: Parsley[Unit] = lexer.lexeme.symbol("skip")
  val ASSIGN_EQ: Parsley[Unit] = lexer.lexeme.symbol("=")
  val READ: Parsley[Unit] = lexer.lexeme.symbol("read")
  val FREE: Parsley[Unit] = lexer.lexeme.symbol("free")
  val RETURN: Parsley[Unit] = lexer.lexeme.symbol("return")
  val EXIT: Parsley[Unit] = lexer.lexeme.symbol("exit")
  val PRINT: Parsley[Unit] = lexer.lexeme.symbol("print")
  val PRINTLN: Parsley[Unit] = lexer.lexeme.symbol("println")
  val IF: Parsley[Unit] = lexer.lexeme.symbol("if")
  val THEN: Parsley[Unit] = lexer.lexeme.symbol("then")
  val ELSE: Parsley[Unit] = lexer.lexeme.symbol("else")
  val FI: Parsley[Unit] = lexer.lexeme.symbol("fi")
  val WHILE: Parsley[Unit] = lexer.lexeme.symbol("while")
  val DO: Parsley[Unit] = lexer.lexeme.symbol("do")
  val DONE: Parsley[Unit] = lexer.lexeme.symbol("done")
  val SEMICOLON: Parsley[Unit] = lexer.lexeme.symbol(";")
  val FST: Parsley[Unit] = lexer.lexeme.symbol("fst")
  val SND: Parsley[Unit] = lexer.lexeme.symbol("snd")
  val NEWPAIR: Parsley[Unit] = lexer.lexeme.symbol("newpair")
  val CALL: Parsley[Unit] = lexer.lexeme.symbol("call")

  // making lexer for Expr branch
  val INT: Parsley[BigInt] = lexer.lexeme.numeric.integer.decimal
  val BOOL: Parsley[Boolean] = lexer.lexeme.symbol("true") #> true | lexer.lexeme.symbol("false") #> false
  val CHAR: Parsley[Char] = lexer.lexeme.text.character.ascii
  val STRING: Parsley[String] = lexer.lexeme.text.string.ascii
  val IDENT: Parsley[String] = lexer.lexeme.names.identifier
  val PAIR_LITER: Parsley[Unit] = lexer.lexeme.symbol("null")


  val OPENSQUAREBRAC: Parsley[Unit] = lexer.lexeme.symbol.openSquare
  val CLOSESQUAREBRAC: Parsley[Unit] = lexer.lexeme.symbol.closingSquare
  val PAIR: Parsley[Unit] = lexer.lexeme.symbol("pair")

  def fully[A](p: Parsley[A]) = lexer.fully(p)

  def symbol(s: String) = lexer.lexeme.symbol(s)

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
}

