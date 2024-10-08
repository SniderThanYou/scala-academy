import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using
import scala.util.parsing.combinator._

class Day02Test extends AnyFunSuite {

  test("Day02") {
    verify_day_02(AoCPart1, "day02_example.txt", 8)
    verify_day_02(AoCPart1, "day02_real.txt", 3099)
    verify_day_02(AoCPart2, "day02_example.txt", 2286)
    verify_day_02(AoCPart2, "day02_real.txt", 72970)
  }

  def verify_day_02(part: AoCPart, input: String, expected: Int) {
    Using.resource(Source.fromResource(input)) { source =>
      val lines = source.getLines()
      val solver = new Day02(part)
      val solution = solver.solve(lines)
      assert(solution == expected)
    }
  }
}
