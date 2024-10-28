import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using
import scala.util.parsing.combinator._

class Day03Test extends AnyFunSuite {

  test("Day03") {
    verify_day_03(AoCPart1, "day03_example.txt", 4361)
    verify_day_03(AoCPart1, "day03_real.txt", 521601)
    verify_day_03(AoCPart2, "day03_example.txt", 467835)
    verify_day_03(AoCPart2, "day03_real.txt", 80694070)
  }

  def verify_day_03(part: AoCPart, input: String, expected: Int) {
    Using.resource(Source.fromResource(input)) { source =>
      val lines = source.getLines()
      val solver = new Day03(part)
      val solution = solver.solve(lines)
      assert(solution == expected)
    }
  }
}
