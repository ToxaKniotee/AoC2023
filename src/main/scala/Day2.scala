import scala.annotation.targetName
import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day2:
  def part1(): Int =
    readInput()
      .filter(_.details.forall(_.isPossible(GameDetails(12, 13, 14))))
      .map(_.id)
      .sum

  def part2(): Long =
    readInput()
      .map(_.details.fold(GameDetails.ZERO)((l, r) => l.minNumber(r)))
      .map(_.power())
      .sum

  case class GameDetails(red: Int, green: Int, blue: Int):
    @targetName("plus")
    infix def +(other: GameDetails): GameDetails = GameDetails(
      this.red + other.red, this.green + other.green, blue + other.blue
    )
    def minNumber(other: GameDetails) : GameDetails =
      GameDetails(
        Math.max(red, other.red),
        Math.max(green, other.green),
        Math.max(blue, other.blue)
      )
    def power(): Long = red * green * blue

    def isPossible(rules: GameDetails): Boolean =
      red <= rules.red &&
        blue <= rules.blue &&
        green <= rules.green

  object GameDetails:
    val ZERO: GameDetails = GameDetails(0, 0, 0)

  case class Game(id: Int, details: List[GameDetails])

  def readInput(): Iterator[Game] = Source.fromResource("day2.txt")
    .getLines()
    .map(readLine)

  def readLine(line: String): Game =
    line match
      case s"Game ${gameId}: $rest" => Game(Integer.parseInt(gameId), Parser.readLine(rest))
      case _                        => throw Exception(s"no game id found for line $line")

  object Parser:
    def readLine(line: String): List[GameDetails] = separateGames(line).map(readGameDetails)

    private def separateGames(line: String): List[String] = line.split("; +").toList

    private def readGameDetails(line: String): GameDetails = line.split(", +")
      .toList
      .map {
        case s"$n red"   => GameDetails(Integer.parseInt(n), 0, 0)
        case s"$n green" => GameDetails(0, Integer.parseInt(n), 0)
        case s"$n blue"  => GameDetails(0, 0, Integer.parseInt(n))
      }
      .fold(GameDetails.ZERO)((l, r) => l + r)
