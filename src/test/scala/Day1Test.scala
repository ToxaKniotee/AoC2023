import munit.FunSuite

class Day1Test extends FunSuite {
  test("day1") {
    val result = Day1.part1()
    println(f"result: $result")
    assertEquals(result, 55386)
  }

  test("day2") {
    val result = Day1.part2()
    println(f"result: $result")
    assert(result > 54807)
    assertEquals(result, 54824)
  }

  test("readLineComplex") {
    assertEquals(Day1.readNumbersComplex("two1nine"), "219")
    assertEquals(Day1.readNumbersComplex("eightwothree"), "823")
  }
}
