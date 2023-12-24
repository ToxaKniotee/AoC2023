import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day3:
  /**
   * The raw result of scanning the
   */
  case class NumberDetails(rowIndex: Int, leftIndex: Int, value: Int, rightIndex: Int):
    def intersects(x: Int, y: Int): Boolean = rowIndex == y && x >= leftIndex && x <= rightIndex

  def part1(value: String = "input/day3.txt"): Int =
    val mainMap = Parser.readInput(value)
    getNumbers(mainMap)
      .filter(number => isPart(mainMap.toMatrix(), number))
      .map(_.value)
      .sum

  def part2(file: String = "input/day3.txt"): Int =
    val map = Parser.readInput(file)
    val numbers = getNumbers(map)
    map
      .flatMap((y, line) => line.toList.zipWithIndex.map((c, x) => (x, y, c)))
      .filter((_, _, c) => c == '*')
      .map((x, y, c) =>
        val neighbours = getNeighbours(x, y, y)
        val tmpNum = numbers
          .filter(number => neighbours.exists((x1, y1) => number.intersects(x1, y1)))
          .map(_.value)
        (x, y, c, tmpNum)
      )
      .filter((_, _, _, numbers) => numbers.size == 2)
      .map((_, _, _, n) => n.product)
      .sum

  private def getNumbers(map: Map[Int, String]): List[NumberDetails] = {
    map
      .flatMap((row, line) => Parser
        .findNumbers(line.toList)
        .map((l, n, r) => NumberDetails(row, l, n, r))
      )
      .toList
  }

  /**
   * Returns all the coordinates that surround a given number
   */
  def getNeighbours(row: Int, l: Int, r: Int): List[(Int, Int)] =
    (l - 1 to r + 1).map((row - 1, _)).toList ++
      List((row, l - 1), (row, r + 1)) ++
      (l - 1 to r + 1).map((row + 1, _))

  /**
   * Checks if a number is a part. It is consider a part if it is adjacent to a symbol that is different that a dot.
   */
  def isPart(map: Map[Int, Map[Int, Char]], number: NumberDetails): Boolean =
    getNeighbours(number.rowIndex, number.leftIndex, number.rightIndex)
      .flatMap((x, y) => map.get(x).flatMap(_.get(y)))
      .exists(char => !char.isDigit && char != '.')

  object Parser:
    def readInput(value: String): Map[Int, String] = Source
      .fromResource(value)
      .getLines()
      .zipWithIndex
      .map((line, x) => x -> line)
      .toMap

    def lineToMap(line: String): Map[Int, Char] =
      line
        .toCharArray
        .zipWithIndex
        .map((char, y) => y -> char)
        .toMap

    /**
     * Recursive function to find the numbers and the left and right index. A number like 234 will have an entry (0, 234,
     * 2)
     *
     * @param line
     * @return
     */
    def findNumbers(line: List[Char], index: Int = 0): List[(Int, Int, Int)] =
      /* Exit condition, no more elements to find in the list */
      if (line.isEmpty) return List()
      /* Remove all the non digit elements */
      val (nonDigit, startOfDigit) = line.span(!_.isDigit)
      val (actualDigit, rest) = startOfDigit.span(_.isDigit)
      val numberValue = String(actualDigit.toArray)
      val left = nonDigit.size + index
      /* We want the index of the lat digit not the next character after that. That's why we are substracting 1 */
      val right = left + actualDigit.size - 1
      if actualDigit.isEmpty then List()
      else List((left, Integer.parseInt(numberValue), right)) ++ Parser.findNumbers(rest, right + 1)

