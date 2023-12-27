import munit.FunSuite

class Day5Test extends FunSuite {
  test("part1") {
    val value = Day5.part1()
    println(value)
    assert(value > 7961058)
    assert(value == 910845529)
  }

  test("part2") {
    println(Day5.part2())
  }

  test("sample part1") {
    println(Day5.part1("sample/day5.txt"))
  }
}
