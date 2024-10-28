import scala.util.parsing.combinator._

class Day04(part: AoCPart) {
  def solve(lines: Iterator[String]): Int = {
    val scratchcards = lines.map(line => Day04Parser.parse(Day04Parser.scratchcard, line)).map(_.get).toSeq
    part match {
      case AoCPart1 =>
        scratchcards.map(_.score).sum
      case AoCPart2 =>
        val ids = scratchcards.map(_.id).sorted
        val idToScratchcard = scratchcards.map(s => s.id -> s).toMap
        val idToCardCount = ids.map(i => i -> 1).to(scala.collection.mutable.Map)
        ids.foreach(id => {
          for (i <- id + 1 to id + idToScratchcard(id).countMatching) {
            idToCardCount(i) += idToCardCount(id)
          }
        })
        idToCardCount.values.sum
    }
  }

  case class Scratchcard(id: Int, winningNumbers: List[Int], numbersYouHave: List[Int]) {
    def score(): Int = {
      countMatching() match {
        case 0 => 0
        case n => math.pow(2, n-1).toInt
      }
    }
    def countMatching(): Int = {
      winningNumbers.filter(n => numbersYouHave.contains(n)).length
    }
  }

  object Day04Parser extends RegexParsers {
    def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def scratchcard: Parser[Scratchcard] = "Card" ~ number ~ ":" ~ rep(number) ~ "|" ~ rep(number) ^^ {
       case _ ~ id ~ _ ~ winNum ~ _ ~ yourNum => Scratchcard(id, winNum.toList, yourNum.toList)
    }
  }
}
