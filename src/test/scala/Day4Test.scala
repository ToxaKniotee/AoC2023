import munit.FunSuite

class Day4Test extends FunSuite {
  test("part1") {
    val result = Day4.part1()
    println(result)
    assert(result == 15268)
  }

  test("part2") {
    val result = Day4.part2()
    println(result)
    assert(result != 68)
    assert(result > 6606)
  }
  
  test("part2.sample") {
    val result = Day4.part2("sample/day4.txt")
    println(result)
  }

}
