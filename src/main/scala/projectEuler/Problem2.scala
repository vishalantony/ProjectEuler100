package projectEuler

object Problem2 extends App {
  val Limit = 4000000

  @scala.annotation.tailrec
  def getFibSum(a: Int, b: Int, sum: Long): Long =
    if(a > Limit) {
      sum
    } else if(a % 2 == 0) {
      getFibSum(b, a + b, sum + a)
    } else {
      getFibSum(b, a + b, sum)
    }

  println(getFibSum(1, 2, 0))
}
