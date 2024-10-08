import scala.util.parsing.combinator._

class Day02(part: AoCPart) {
  def solve(lines: Iterator[String]): Int = {
    val games = lines.map(line => Day02Parser.parse(Day02Parser.game, line)).map(_.get)
    part match {
      case AoCPart1 =>
        games.filter(possible).map(_.id).sum
      case AoCPart2 =>
        games.map(minPower).sum
    }
  }

  private val possibleCubes = ColorCount(12, 13, 14)

  private def possible(game: Game): Boolean = {
    game.handfuls.forall(possible)
  }

  private def possible(colorCount: ColorCount): Boolean = {
    colorCount.r <= possibleCubes.r && colorCount.g <= possibleCubes.g && colorCount.b <= possibleCubes.b
  }

  private def minPower(game: Game): Int = {
    game.handfuls.foldLeft(ColorCount.ZERO)(_.max(_)).power
  }

  case class ColorCount(r: Int, g: Int, b: Int) {
    def +(other: ColorCount): ColorCount = ColorCount(r + other.r, g + other.g, b + other.b)
    def max(other: ColorCount): ColorCount = ColorCount(r.max(other.r), g.max(other.g), b.max(other.b))
    def power(): Int = r * g * b
  }

  object ColorCount {
    val ZERO: ColorCount = ColorCount(0, 0, 0)
  }

  case class Game(id: Int, handfuls: List[ColorCount])

  object Day02Parser extends RegexParsers {
    def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def gameId: Parser[Int] = "Game" ~ number ^^ { case g ~ n => n }
    def color: Parser[String] = """(red|green|blue)""".r
    def colorCount: Parser[ColorCount] = number ~ color ^^ {
      case n ~ c => c match {
        case "red" => ColorCount(n, 0, 0)
        case "green" => ColorCount(0, n, 0)
        case "blue" => ColorCount(0, 0, n)
      }
    }
    def handful: Parser[ColorCount] = repsep(colorCount, ",") ^^ {
      seq => seq.foldLeft(ColorCount.ZERO)(_ + _)
    }
    def game: Parser[Game] = gameId ~ ":" ~ repsep(handful, ";") ^^ {
      case gameId ~ _ ~ seq => Game(gameId, seq)
    }
  }
}
