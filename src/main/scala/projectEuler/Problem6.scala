package projectEuler

object Problem6 extends App {
  // Solution 1
  def bruteForceSolution(n: Int): Int =
    sq((1 to n).sum) - (1 to n).map(sq).sum

  // Solution 2
  def sumOfSquaresUpto(n: Int): Int =
    n * (n + 1) * (2 * n + 1) / 6

  def sq(n: Int): Int = n * n

  def squareOfSumUpto(n: Int): Int =
    sq(n * (n + 1) / 2)

  def diffBetweenSquareOfSumAndSumOfSquaresUpto_solution1(n: Int): Int =
    squareOfSumUpto(100) - sumOfSquaresUpto(100)

  // Solution 3
  def diffBetweenSquareOfSumAndSumOfSquaresUpto(n: Int): Int =
    n * (n + 1) * (3 * n + 2) * (n - 1) / 12

  println(bruteForceSolution(100))
  println(diffBetweenSquareOfSumAndSumOfSquaresUpto_solution1(100))
  println(diffBetweenSquareOfSumAndSumOfSquaresUpto(100))
}
