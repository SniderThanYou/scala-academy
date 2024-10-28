import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using
import scala.util.parsing.combinator._

class Day04Test extends AnyFunSuite {

  test("Day04") {
    verify_day_04(AoCPart1, "day04_example.txt", 13)
    verify_day_04(AoCPart1, "day04_real.txt", 25174)
    verify_day_04(AoCPart2, "day04_example.txt", 30)
    verify_day_04(AoCPart2, "day04_real.txt", 6420979)
  }

  def verify_day_04(part: AoCPart, input: String, expected: Int) {
    Using.resource(Source.fromResource(input)) { source =>
      val lines = source.getLines()
      val solver = new Day04(part)
      val solution = solver.solve(lines)
      assert(solution == expected)
    }
  }
}
