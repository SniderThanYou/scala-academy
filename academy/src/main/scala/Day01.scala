class Day01(part: AoCPart) {
  def solve(lines: Iterator[String]): Int = {
    lines.map(s => parseCoordinate(s)).sum
  }

  private val singleDigitsToValues = scala.collection.immutable.HashMap(
    "0" -> 0,
    "1" -> 1,
    "2" -> 2,
    "3" -> 3,
    "4" -> 4,
    "5" -> 5,
    "6" -> 6,
    "7" -> 7,
    "8" -> 8,
    "9" -> 9,
  )

  private val wordsToValues = scala.collection.immutable.HashMap(
    "zero" -> 0,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  val digitsToValues = part match {
    case AoCPart1 => singleDigitsToValues
    case AoCPart2 => singleDigitsToValues ++ wordsToValues
  }

  private def parseCoordinate(line: String): Int = {
    val digits = collectDigits(line)
    10 * digits.head + digits.last
  }

  private def collectDigits(line: String): List[Int] = {
    line.zipWithIndex.map(tuple => line.substring(tuple._2)).map(digitAtStart).filter(_.nonEmpty).map(_.get).toList
  }

  private def digitAtStart(substring: String): Option[Int] = {
    digitsToValues.find(kvp => substring.startsWith(kvp._1)).map(_._2)
  }

  private def isDigitAtIndex(line: String, index: Int): Boolean = {
    val substr = line.substring(index)
    digitsToValues.keys.exists(k => substr.startsWith(k))
  }
}
