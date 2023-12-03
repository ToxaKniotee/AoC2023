import scala.io.Source

object Day1:
  def part1(): Int =
    Source.fromResource("day1.txt")
      .getLines()
      .map(readNumbers)
      .sum

  def part2(): Int = Source
    .fromResource("day1.txt")
    .getLines()
    .map(readNumbersComplex)
    .map(getFirstAndLat)
    .sum

  private def getFirstAndLat(xs: String) =
    val left = xs.head
    val right = xs.last
    StringBuilder()
      .append(left)
      .append(right)
      .toString()
      |> Integer.parseInt

  private def readNumbers(line: String): Int =
      val digits = line.filter(_.isDigit)
      val left = digits.head
      val right = digits.last
      StringBuilder()
        .append(left)
        .append(right)
        .toString()
        |> Integer.parseInt

  def readNumbersComplex(line: String): String =
    line match
      case s"one$x"   => "1" + readNumbersComplex(line.tail)
      case s"two$x"   => "2" + readNumbersComplex(line.tail)
      case s"three$x" => "3" + readNumbersComplex(line.tail)
      case s"four$x"  => "4" + readNumbersComplex(line.tail)
      case s"five$x"  => "5" + readNumbersComplex(line.tail)
      case s"six$x"   => "6" + readNumbersComplex(line.tail)
      case s"seven$x" => "7" + readNumbersComplex(line.tail)
      case s"eight$x" => "8" + readNumbersComplex(line.tail)
      case s"nine$x"  => "9" + readNumbersComplex(line.tail)
      /* Empty string*/
      case "" => ""
      /* Regular number */
      case _ =>
        if line.head.isDigit
        then line.head + readNumbersComplex(line.tail)
        else readNumbersComplex(line.tail)