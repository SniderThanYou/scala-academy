import scala.util.parsing.combinator._

case class ColorCount(count: Int, color: String)
case class Handful(colorCounts: List[ColorCount])
case class Game(id: Int, handfuls: List[Handful])

object Day02Parser extends RegexParsers {
  def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def gameId: Parser[Int] = "Game" ~ number ^^ { case g ~ n => n }
  def color: Parser[String] = """(red|green|blue)""".r
  def colorCount: Parser[ColorCount] = number ~ color ^^ { case n ~ c => ColorCount(n, c) }
  def handful: Parser[Handful] = repsep(colorCount, ",") ^^ { seq => Handful(seq) }
  def game: Parser[Game] = gameId ~ ":" ~ repsep(handful, ";") ^^ { case gameId ~ _ ~ seq => Game(gameId, seq)}
}

class Day02(part: AoCPart) {
  def solve(lines: Iterator[String]): Int = {
    val games = lines.map(line => Day02Parser.parse(Day02Parser.game, line)).map(_.get)
    part match {
      case AoCPart1 => {
        val impossibleGames = games.collect { case g if possible(g) => g }
        impossibleGames.map(_.id).sum
      }
      case AoCPart2 => {
        games.map(minPower).sum
      }
    }
  }

  private val possibleCubes = scala.collection.immutable.HashMap(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14,
  )

  private def possible(game: Game): Boolean = {
    game.handfuls.forall(possible)
  }

  private def possible(handful: Handful): Boolean = {
    handful.colorCounts.forall(possible)
  }

  private def possible(colorCount: ColorCount): Boolean = {
    colorCount.count <= possibleCubes(colorCount.color)
  }

  private def minPower(game: Game): Int = {
    var r = 0
    var g = 0
    var b = 0
    game.handfuls.foreach(h => {
      h.colorCounts.foreach(cc => {
          cc.color match {
            case "red" => r = r.max(cc.count)
            case "green" => g = g.max(cc.count)
            case "blue" => b = b.max(cc.count)
          }
      })
    })
    r * g * b
  }
}
