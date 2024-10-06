import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using

class Day01Test extends AnyFunSuite {

  test("Day01") {
    verify_day_01(AoCPart1, "day01_part1_example.txt", 142)
    verify_day_01(AoCPart1, "day01_part1_real.txt", 54304)
    verify_day_01(AoCPart2, "day01_part2_example.txt", 281)
    verify_day_01(AoCPart2, "day01_part1_real.txt", 54418)
  }

  def verify_day_01(part: AoCPart, input: String, expected: Int) {
    Using.resource(Source.fromResource(input)) { source =>
      val lines = source.getLines()
      val solver = new Day01(part)
      val solution = solver.solve(lines)
      assert(solution == expected)
    }
  }
}
