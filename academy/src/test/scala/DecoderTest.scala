import org.scalatest.funsuite.AnyFunSuite
import scala.util.parsing.combinator._

object DecoderParser extends RegexParsers {
  def letters: Parser[String] = """[a-z]+""".r
  def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def bracketedContent: Parser[String] = number ~ "[" ~ expression ~ "]" ^^ {
    case num ~ _ ~ expr ~ _ => expr * num
  }
  def expression: Parser[String] = rep(letters | bracketedContent) ^^ { _.mkString }
  def parseInput(input: String): ParseResult[String] = parseAll(expression, input)
}

class DecoderTest extends AnyFunSuite {

  test("Decoder") {
    verifyDecoder("a", "a")
    verifyDecoder("ab", "ab")
    verifyDecoder("1[a]", "a")
    verifyDecoder("3[a]", "aaa")
    verifyDecoder("b3[a]", "baaa")
    verifyDecoder("b3[a]b", "baaab")
    verifyDecoder("b3[a]2[b]", "baaabb")
    verifyDecoder("b3[abc]2[bde]", "babcabcabcbdebde")
    verifyDecoder("b3[ab2[c]]", "babccabccabcc")
  }

  def verifyDecoder(encoded: String, expected: String) {
    assert(DecoderParser.parseInput(encoded).get == expected)
  }
}
