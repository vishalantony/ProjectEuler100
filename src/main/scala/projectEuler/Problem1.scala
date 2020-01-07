package projectEuler

object Problem1 extends App {

  def getAllMultiplesOf3or5(below: Int): Seq[Int] =
    (1 until below).filter(x => (x % 3 == 0) || (x % 5 == 0))

  // Brute force solution
  def getSumOfAllMultiplesOf3or5_bruteForce(below: Int): Int =
    getAllMultiplesOf3or5(below).sum

  def sumOfMultiplesTill(till: Int)(n: Int): Int =
    n * (till/n) * (till/n + 1) / 2

  // Constant time solution
  def getSumOfAllMultiplesOf3or5(below: Int): Int = {
    val sumOfMultiplesOf = sumOfMultiplesTill(below - 1)(_)
    sumOfMultiplesOf(3) + sumOfMultiplesOf(5) - sumOfMultiplesOf(15)
  }

  println(getSumOfAllMultiplesOf3or5(1000))
}

