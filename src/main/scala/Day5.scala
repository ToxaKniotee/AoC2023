import scala.io.Source

object Day5:
  def part1(file: String = "input/day5.txt"): BigInt =
    val (seeds, function) = Parser.parse(file)
    seeds
      .map(function)
      .min

  def part2(file: String = "input/day5.txt"): BigInt =
    val (seeds, function) = Parser.parse(file)
    def min(a: BigInt, b: BigInt): BigInt =
      if a < b then a else b
    def getMinRange(number: BigInt, range: BigInt): BigInt =
      var minNumber = function(number)
      for i <- number + 1 to number + range do
        minNumber = min(minNumber, function(i))
      minNumber
    def getMinRanges(numbers: List[BigInt]): BigInt =
      numbers match
        case number :: range :: Nil =>  getMinRange(number, range)
        case number :: range :: rest => min(getMinRange(number, range), getMinRanges(rest))
    getMinRanges(seeds)

  object Parser:
    def parse(file: String): (List[BigInt], BigInt => BigInt) =
      val lines = Source.fromResource(file).getLines().toList
      /* First line has seeds */
      val seeds = lines.head.drop("seeds: ".length).split(" ").map(BigInt(_)).toList
      val functions = lines.drop(2) |> parseFunctions
      (seeds, (n: BigInt) => functions.foldLeft(n)((n1, f) => f(n1)))

    def parseFunctions(values: List[String]): List[BigInt => BigInt] =
      val (left, rest) = values.span(!_.isBlank)
      if left.isEmpty then List()
      else parseFunction(left) :: parseFunctions(rest.safeTail)

    def parseFunction(values: List[String]): BigInt => BigInt =
      def parseLine(line: String): (BigInt, BigInt, BigInt) =
        val targetStart :: sourceStart :: range :: Nil = line.split(" ").map(BigInt(_)).toList
        (sourceStart, targetStart, range)

      val mapFunction = values.tail.map(parseLine)
      sourceNumber =>
        mapFunction
          .find((source, target, range) => sourceNumber.between(source, source + range))
          .map((source, target, range) => sourceNumber - source + target)
          .getOrElse(sourceNumber)
          //<|> (n => println(s"Executing ${values.head} $sourceNumber -> $n"))