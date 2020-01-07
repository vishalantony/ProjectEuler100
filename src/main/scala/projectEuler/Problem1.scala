package projectEuler

object Problem1 extends App {

  def getAllMultiplesOf3or5(below: Int): Seq[Int] =
    (1 until below).filter(x => (x % 3 == 0) || (x % 5 == 0))

  def getSumOfAllMultiplesOf3or5(below: Int): Int =
    getAllMultiplesOf3or5(below).sum

  println(getSumOfAllMultiplesOf3or5(1000))
}

