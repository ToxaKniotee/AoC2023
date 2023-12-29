import scala.math.BigDecimal.RoundingMode

object Day6:
  val values: List[(Int, Int)] = List((34, 204), (90, 1713), (89, 1210), (86, 1780))

//  def part1: Int =
//    values
//      .map((l, r) => (BigInt(l), BigInt(r)))
//      .map(calculateRoot.tupled)
//      .map(values =>
//        val l :: h :: Nil = values
//        Math.floor(h) - Math.ceil(l) + 1
//      )
//      .product
//      .toInt

  def part2: BigDecimal =
    val l :: h :: Nil = calculateRoot(34908986, BigInt("204171312101780"))
    h.setScale(0, RoundingMode.FLOOR) - l.setScale(0, RoundingMode.CEILING) + 1

  def calculateRoot(time: BigInt, distance: BigInt): List[BigDecimal] =
    /* b^2 - 4*/
    val discriminant = BigDecimal(new java.math.BigDecimal((time.pow(2) - 4 * -1 * - distance).toString()).sqrt(java.math.MathContext.DECIMAL32).toString)
    List(
      (-time.toBigDecimal + discriminant) / -2,
      (-time.toBigDecimal - discriminant) / -2
    ).sorted