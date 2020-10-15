import collection.mutable.Stack
import org.scalatest.flatspec.AnyFlatSpec
import Solution._

class SolutionTest extends AnyFlatSpec {

  it should "solve first case" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('-', '-', '-'),
      Array('+', '-', '+')
    )
    val words = Array("AND", "ANT")
    assert(1 == solve(matrix, words, 0))
  }
  it should "solve second case" in {
    val matrix = Array(
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '-', '-', '-', '-', '+', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '+', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '+', '-', '-', '-', '-', '-', '-', '+', '+'),
      Array('+', '+', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '+', '+', '+', '+', '-', '+', '+', '+', '+')
    )
    val words = Array("LONDON", "DELHI", "ICELAND", "ANKARA")
    assert(1 == solve(matrix, words, 0))
  }
  it should "solve third case" in {
    val matrix = Array(
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '-', '-', '-', '-', '-', '-', '+', '+'),
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '+', '+', '+', '+', '+'),
      Array('+', '-', '-', '-', '-', '-', '-', '+', '+', '+'),
      Array('+', '-', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '+', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '+', '+', '+', '+', '-', '+', '+', '+', '+'),
      Array('+', '+', '+', '+', '+', '+', '+', '+', '+', '+')
    )
    val words = Array("AGRA", "ENGLAND", "NORWAY", "GWALIOR")
    assert(1 == solve(matrix, words, 0))
  }

  /*
  horizontal fitting checks
  */
  "xxx" should "fit in (+++,---,+++) horizontally" in {
    val matrix = Array(
      Array('+', '+', '+'),
      Array('-', '-', '-'),
      Array('+', '+', '+')
    )
    assert(validHorizontalPosition(matrix, word = "xxx", i = 1, j = 0))
  }

  "xx" should "fit in (+++,+--,+++) horizontally" in {
    val matrix = Array(
      Array('+', '+', '+'),
      Array('+', '-', '-'),
      Array('+', '+', '+')
    )
    assert(validHorizontalPosition(matrix, word = "xx", i = 1, j = 1))
  }

  "xxx" should "fit in (+++,--x,+++) horizontally" in {
    val matrix = Array(
      Array('+', '+', '+'),
      Array('-', '-', 'x'),
      Array('+', '+', '+')
    )
    assert(validHorizontalPosition(matrix, word = "xxx", i = 1, j = 0))
  }

  "xxx" should "not fit in (+++,--y,+++) horizontally" in {
    val matrix = Array(
      Array('+', '+', '+'),
      Array('-', '-', 'y'),
      Array('+', '+', '+')
    )
    assert(!validHorizontalPosition(matrix, word = "xxx", i = 1, j = 0))
  }

  "xxxx" should "not fit in (+++,---,+++) horizontally" in {
    val matrix = Array(
      Array('+', '+', '+'),
      Array('-', '-', '-'),
      Array('+', '+', '+')
    )
    assert(!validHorizontalPosition(matrix, word = "xxxx", i = 1, j = 0))
  }
  "xx" should "not fit in (+++,---,+++) horizontally" in {
    val matrix = Array(
      Array('+', '+', '+'),
      Array('-', '-', 'x'),
      Array('+', '+', '+')
    )
    assert(!validHorizontalPosition(matrix, word = "xx", i = 1, j = 0))
  }

  /*
  vertical fitting checks
  */
  "xxx" should "fit in (+-+,+-+,+-+) vertically" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('+', '-', '+'),
      Array('+', '-', '+')
    )
    assert(validVerticalPosition(matrix, word = "xxx", i = 0, j = 1))
  }

  "xx" should "fit in (+-+,+-+,+++) vertically" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('+', '-', '+'),
      Array('+', '+', '+')
    )
    assert(validVerticalPosition(matrix, word = "xx", i = 0, j = 1))
  }

  "xxx" should "fit in (+-+,+-+,+x+) vertically" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('+', '-', '+'),
      Array('+', 'x', '+')
    )
    assert(validVerticalPosition(matrix, word = "xxx", i = 0, j = 1))
  }

  "xxx" should "not fit in (+-+,+-+,+y+) vertically" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('+', '-', '+'),
      Array('+', 'y', '+')
    )
    assert(!validVerticalPosition(matrix, word = "xxx", i = 0, j = 1))
  }

  "xxxx" should "not fit in (+-+,+-+,+-+) vertically" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('+', '-', '+'),
      Array('+', '-', '+')
    )
    assert(!validVerticalPosition(matrix, word = "xxxx", i = 0, j = 1))
  }
  "xx" should "not fit in (+-+,+-+,+x+) vertically" in {
    val matrix = Array(
      Array('+', '-', '+'),
      Array('+', '-', '+'),
      Array('+', 'x', '+')
    )
    assert(!validVerticalPosition(matrix, word = "xx", i = 1, j = 0))
  }

}