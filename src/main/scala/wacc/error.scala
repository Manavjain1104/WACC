package wacc

import parsley.errors.{ErrorBuilder, tokenextractors}
import wacc.SemTypes._

object error {

  sealed trait WaccError

  //Syntax Error for parsley
  sealed trait SyntaxErrorLines

  case class VanillaError(unexpected: Option[SyntaxErrorItem],
                          expected: Set[SyntaxErrorItem], reasons: List[String],
                          line: LineInfo) extends SyntaxErrorLines

  case class SpecialisedError(msgs: List[String], line: LineInfo) extends SyntaxErrorLines

  sealed trait SyntaxErrorItem

  case class SyntaxRaw(item: String) extends SyntaxErrorItem

  case class SyntaxNamed(item: String) extends SyntaxErrorItem

  case object SyntaxEndOfInput extends SyntaxErrorItem

  case class LineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int)

  case class SyntaxError(pos: (Int, Int), source: Option[String], lines: SyntaxErrorLines) extends WaccError

  class SyntaxErrorBuilder extends ErrorBuilder[SyntaxError] with tokenextractors.TillNextWhitespace {
    override def format(pos: Position, source: Source, lines: SyntaxErrorLines): SyntaxError = {
      SyntaxError(pos, source, lines)
    }

    override type Position = (Int, Int)
    override type Source = Option[String]

    override def pos(line: Int, col: Int): Position = (line, col)

    override def source(sourceName: Option[String]): Source = sourceName

    override type ErrorInfoLines = SyntaxErrorLines

    override def vanillaError(unexpected: Option[SyntaxErrorItem],
                              expected: Set[SyntaxErrorItem], reasons: List[String],
                              line: LineInfo): ErrorInfoLines = {
      VanillaError(unexpected, expected, reasons, line)
    }

    override def specialisedError(msgs: Messages, line: error.LineInfo): SyntaxErrorLines = {
      SpecialisedError(msgs, line)
    }

    override type ExpectedItems = Set[SyntaxErrorItem]
    override type Messages = List[String]

    override def combineExpectedItems(alts: Set[SyntaxErrorItem]): ExpectedItems = alts

    override def combineMessages(alts: Seq[Message]): Messages = alts.toList

    override type UnexpectedLine = Option[SyntaxErrorItem]
    override type ExpectedLine = Set[SyntaxErrorItem]
    override type Message = String
    override type LineInfo = error.LineInfo

    override def unexpected(item: Option[Item]): UnexpectedLine = item

    override def expected(alts: ExpectedItems): ExpectedLine = alts

    override def reason(reason: String): Message = reason

    override def message(msg: String): Message = msg

    override def lineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int): LineInfo = {
      LineInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth)
    }

    override val numLinesBefore: Int = 2
    override val numLinesAfter: Int = 2
    override type Item = SyntaxErrorItem
    override type Raw = SyntaxRaw
    override type Named = SyntaxNamed
    override type EndOfInput = SyntaxEndOfInput.type

    override def raw(item: String): Raw = SyntaxRaw(item)

    override def named(item: String): Named = SyntaxNamed(item)

    override val endOfInput: EndOfInput = SyntaxEndOfInput

    override def trimToParserDemand: Boolean = false
  }

  // Semantic Error Hierarchy
  sealed trait SemanticError extends WaccError

  case class UnknownIdentifierError(pos: (Int, Int), ident: String, context: Option[String]) extends SemanticError

  case class InvalidStructTypeError(pos : (Int, Int), foundType : SemType, context: Option[String]) extends SemanticError

  case class TypeError(pos: (Int, Int), expectedTypes: Set[SemType], foundType: SemType, context: Option[String])(var offset: Int) extends SemanticError

  object TypeError {
    def apply(pos: (Int, Int), value: Set[SemTypes.SemType], semType: SemTypes.SemType, context: Option[String])(offset: Int): TypeError = {
      new TypeError(pos, value, semType, context)(offset)
    }

    def apply(pos: (Int, Int), expectedTypes: Set[SemType], foundType: SemType, context: Option[String]): TypeError = {
      new TypeError(pos, expectedTypes, foundType, context)(0)
    }
  }

  case class TypeErasureError(pos: (Int, Int), context: Option[String]) extends SemanticError

  case class UnknownObjectError(pos: (Int, Int), context: Option[String]) extends SemanticError

  case class InvalidScopeError(pos: (Int, Int), member : String, context: Option[String]) extends SemanticError

  case class ArityMismatch(pos: (Int, Int), expectedArity: Int, foundArity: Int, context: Option[String]) extends SemanticError

  /*out of bounds or dimension error*/
  case class ArrayError(pos: (Int, Int), arrName: String, maxDimension: Int, context: Option[String]) extends SemanticError

  case class DuplicateIdentifier(pos: (Int, Int), ident: String, context: Option[String]) extends SemanticError

  case class InvalidReturnError(pos: (Int, Int), context: Option[String]) extends SemanticError

}