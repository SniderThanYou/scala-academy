import scala.util.parsing.combinator._

class Day03(part: AoCPart) {
  def solve(lines: Iterator[String]): Int = {
    val schematic = parseSchematic(lines)
    part match {
      case AoCPart1 =>
        schematic.partNumbers().map(_.n).sum
      case AoCPart2 =>
        schematic.gearRatios().sum
    }
  }

  case class Coord(x: Int, y: Int)
  case class BoundingBox(left: Int, right: Int, bottom: Int, top: Int) {
    def contains(coord: Coord): Boolean = {
      coord.x >= left && coord.x <= right && coord.y >= bottom && coord.y <= top
    }
  }
  case class ParsedNumber(n: Int, coord: Coord, length: Int) {
    def neighborOf(symbol: ParsedSymbol): Boolean = {
      BoundingBox(coord.x - 1, coord.x + length, coord.y - 1, coord.y + 1).contains(symbol.coord)
    }
  }
  case class ParsedSymbol(sym: String, coord: Coord)

  case class Schematic(numbers: List[ParsedNumber] = List(), symbols: List[ParsedSymbol] = List()) {
    def +(other: Schematic): Schematic = Schematic(numbers ++ other.numbers, symbols ++ other.symbols)
    def partNumbers(): Iterable[ParsedNumber] = {
      numbers.filter(hasSymbolNeighbor)
    }
    def hasSymbolNeighbor(number: ParsedNumber): Boolean = {
      symbols.exists(s => number.neighborOf(s))
    }
    def gearRatios(): Iterable[Int] = {
      symbols.filter(_.sym == "*").map(gearRatio)
    }
    def gearRatio(symbol: ParsedSymbol): Int = {
      val neighbors = numbers.filter(n => n.neighborOf(symbol))
      if (neighbors.length == 2) {
        neighbors.map(_.n).product
      } else {
        0
      }
    }
  }

  private def parseSchematic(lines: Iterator[String]): Schematic = {
    lines.zipWithIndex.map{case (line, i) => parseLine(line, i)}.foldLeft(Schematic(List(), List()))(_ + _)
  }

  val numberPattern = """\d+""".r
  val symbolPattern = """[^\.\d]""".r
  private def parseLine(line: String, lineNumber: Int): Schematic = {
    Schematic(
      numberPattern.findAllMatchIn(line).map(m => ParsedNumber(m.matched.toInt, Coord(m.start, lineNumber), m.end - m.start)).toList,
      symbolPattern.findAllMatchIn(line).map(m => ParsedSymbol(m.matched, Coord(m.start, lineNumber))).toList,
    )
  }
}
