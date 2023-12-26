import scala.io.Source

object Day4:
  case class Scratchpad(number: Int, winningNumbers: Set[Int], numbers: List[Int]):
    def getWinnings: List[Int] = numbers.filter(winningNumbers.contains)

  def part1(): Int =
    Parser
      .parseInput()
      .map(scratchpad => scratchpad.numbers.filter(n => scratchpad.winningNumbers.contains(n)))
      .map(numbers => Math.pow(2, numbers.size - 1).toInt)
      .sum

  def part2(value: String = "input/day4.txt"): Int =
    def processWinnings(total: List[Int], scratchpads: List[Scratchpad]): List[Int] =
      if scratchpads.isEmpty then
        /* One for this card only */
        return List()
      end if
      val currentCounter = total.head
      val scratchpad = scratchpads.head
      val winnings = scratchpad.getWinnings
      if winnings.isEmpty then
        return List(total.head + 1) ++ processWinnings(
          if total.size == 1 then List(0) else total.tail,
          scratchpads.tail
        )
      end if
      val next = List
        .fill(winnings.size)(total.head + 1)
        .zipAll(total.tail, 0, 0)
        .map(_+_)
      List(currentCounter + 1) ++ processWinnings(next, scratchpads.tail)
    processWinnings(List(0), Parser.parseInput(value))
      .sum

  object Parser:
    private def getNumbers(line: String): List[Int] =
      line
        .split("\\s+")
        .filter(!_.isBlank)
        .toList
        .map(Integer.parseInt)

    private def parseLine(line: String): Scratchpad =
      line match
        case s"Card $n: $winningNumbers | $numbers" => Scratchpad(
          Integer.parseInt(n.strip()),
          getNumbers(winningNumbers).toSet,
          getNumbers(numbers)
        )
        case _ => throw IllegalArgumentException(s"Unable to parse line $line")

    def parseInput(file: String = "input/day4.txt"): List[Scratchpad] =
      Source.fromResource(file)
        .getLines()
        .map(parseLine)
        .toList