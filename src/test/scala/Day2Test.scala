import munit.FunSuite

class Day2Test extends FunSuite {
  test("parser") {
    val line = Day2.readLine("Game 12: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    println(line)
  }

  test("part1") {
    println(Day2.part1());
  }

  test("part2") {
    val result = Day2.part2()
    println(result);
    assert(result > 17397)
  }
}
