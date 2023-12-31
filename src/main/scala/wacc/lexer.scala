package wacc

import parsley.Parsley
import parsley.Parsley.{attempt, notFollowedBy}
import parsley.character.{char, digit, string, whitespace}
import parsley.errors.combinator.ErrorMethods
import parsley.token.Lexer
import parsley.token.descriptions.NameDesc
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.symbol.ImplicitSymbol

import java.lang.Character.isWhitespace

object lexer {

  import parsley.token.{descriptions, predicate}
  import descriptions.{LexicalDesc, SpaceDesc, SymbolDesc}

  def identStart(c: Char): Boolean    = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  def identContinue(c: Char): Boolean = (c >= '0' && c <= '9') || identStart(c)

  def graphicChar(c: Char): Boolean   = ((c >= ' ') && (!Set('\\', '\'', '\"').contains(c))) || escapes.contains(c)

  def isSpace(c: Char): Boolean       = c == '\n' || isWhitespace(c.toInt)

  def validInt(x: Int): Boolean       = (x <= Int.MaxValue) && (x >= Int.MinValue)



  val escapes = Set('\u0000', '\b', '\t', '\f', '\r')



  private val desc = LexicalDesc.plain.copy(
    nameDesc           = NameDesc.plain.copy(
      identifierStart  = predicate.Basic(identStart),
      identifierLetter = predicate.Basic(identContinue)
    ),
    spaceDesc              = SpaceDesc.plain.copy(
      commentLine          = "#",
      commentLineAllowsEOF = true,
      space                = predicate.Basic(isSpace)
    ),
    symbolDesc     = SymbolDesc.plain.copy(
      hardKeywords = Set[String]("begin", "end", "is", "skip", "free", "read",
        "return", "exit", "print", "println", "if", "then", "else", "fi", "while", "do", "done",
        "snd", "fst", "newpair", "call", "int", "bool", "char", "string", "pair", "true", "false",
        "null", "public", "private", "class", "this", "new", "match", "case"
      ),
      hardOperators = Set[String]("!", "-", "len", "ord", "chr", "*", "/", "%", "+", ">", ">=",
        "<", "<=", "==", "!=", "&&", "||"),
      caseSensitive = true
    ),
    textDesc           = TextDesc.plain.copy(
      escapeSequences  = EscapeDesc.plain.copy(
        escBegin       = '\\',
        literals       = Set('\'', '\"', '\\'),
        singleMap      = Map(
          '0' -> 0x0000,
          'b' -> 0x0008,
          'f' -> 0x000c,
          'n' -> 0x000a,
          'r' -> 0x000d,
          't' -> 0x0009
        ),
      ),
      multiStringEnds  = Set.empty,
      graphicCharacter = predicate.Basic(graphicChar)
    ),
  )

  private val lexer = new Lexer(desc)

  // token definitions
  val BEGIN: Parsley[Unit] = symbol("begin")
  val END: Parsley[Unit] = symbol("end")
  val OPENPAREN: Parsley[Unit] = lexer.lexeme.symbol.openParen.label("\'(\'")
  val CLOSEDPAREN: Parsley[Unit] = lexer.lexeme.symbol.closingParen.label("\')\'")
  val IS: Parsley[Unit] = symbol("is")
  val COMMA: Parsley[Unit] = symbol(",")
  val SKIP: Parsley[Unit] = symbol("skip")
  val ASSIGN_EQ: Parsley[Unit] = symbol("=")
  val READ: Parsley[Unit] = symbol("read")
  val FREE: Parsley[Unit] = symbol("free")
  val RETURN: Parsley[Unit] = symbol("return")
  val EXIT: Parsley[Unit] = symbol("exit")
  val PRINT: Parsley[Unit] = symbol("print")
  val PRINTLN: Parsley[Unit] = symbol("println")
  val IF: Parsley[Unit] = symbol("if")
  val THEN: Parsley[Unit] = symbol("then")
  val ELSE: Parsley[Unit] = symbol("else")
  val FI: Parsley[Unit] = symbol("fi")
  val WHILE: Parsley[Unit] = symbol("while")
  val DO: Parsley[Unit] = symbol("do")
  val DONE: Parsley[Unit] = symbol("done")
  val SEMICOLON: Parsley[Unit] = symbol(";")
  val FST: Parsley[Unit] = symbol("fst")
  val SND: Parsley[Unit] = symbol("snd")
  val NEWPAIR: Parsley[Unit] = symbol("newpair")
  val CALL: Parsley[Unit] = symbol("call")
  val MATCH: Parsley[Unit] = symbol("match")
  val CASE: Parsley[Unit] = symbol("case")
  val MATCHARROW: Parsley[Unit] = symbol("=>")
  val COLON: Parsley[Unit] = symbol(":")
  val PUBLIC: Parsley[Unit]       = symbol("public")
  val PRIVATE: Parsley[Unit]      = symbol("private")
  val CLASS: Parsley[Unit]        = symbol("class")
  val STRUCT: Parsley[Unit]       = symbol("struct")
  val THIS: Parsley[Unit]         = symbol("this")
  val NEW: Parsley[Unit]          = symbol("new")
  val OPENCURLY: Parsley[Unit]    = lexer.lexeme.symbol.openBrace.label("\'{\'")
  val CLOSEDCURLY: Parsley[Unit]  = lexer.lexeme.symbol.closingBrace.label("\'}\'")
  val DOT: Parsley[Unit]          = symbol(".")


  // making lexer for Expr branch
  val INT: Parsley[Int] = lexer.lexeme.numeric.signed.decimal32.filter(validInt).explain("Only 32-bit signed intergers allowed ")
  val BOOL: Parsley[Boolean] = symbol("true") #> true | symbol("false") #> false
  val CHAR: Parsley[Char] = lexer.lexeme.text.character.ascii
  val STRING: Parsley[String] = lexer.lexeme.text.string.ascii
  val IDENT: Parsley[String] = lexer.lexeme.names.identifier
  val PAIR_LITER: Parsley[Unit] = symbol("null")
  val VOID: Parsley[Unit] = symbol("void")

  val OPENSQUAREBRAC: Parsley[Unit] = lexer.lexeme.symbol.openSquare.label("\'[\'")
  val CLOSESQUAREBRAC: Parsley[Unit] = lexer.lexeme.symbol.closingSquare.label("\']\'")
  val ARROW: Parsley[Unit] = lexer.lexeme.symbol("->")
  val PAIR: Parsley[Unit] = symbol("pair")

  val NEGATE: Parsley[Unit] = lexer.lexeme(attempt(char('-') *> notFollowedBy(digit)))
  val LEN: Parsley[String]  = lexer.lexeme(attempt(string("len") <* whitespace))
  val ORD: Parsley[String]  = lexer.lexeme(attempt(string("ord") <* whitespace))
  val CHR: Parsley[String]  = lexer.lexeme(attempt(string("chr") <* whitespace))

  def fully[A](p: Parsley[A]) = lexer.fully(p)

  def symbol(s: String) = lexer.lexeme.symbol(s)

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
}
