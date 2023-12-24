import munit.FunSuite

class Day3Test extends FunSuite{
  test("part1") {
    val result = Day3.part1()
    println(result)
    assert(result > 236428)
    assert(result == 527369)
  }

  test("part2") {
    println(Day3.part2())
  }

  test("sample1") {
    println(Day3.part1("sample/day3.txt"))
  }
}
