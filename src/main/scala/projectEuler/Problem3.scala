package projectEuler

object Problem3 extends App {

  @scala.annotation.tailrec
  def removeFactor(n: Long, i: Long): Long =
    if(n % i == 0) {
      removeFactor(n/i, i)
    } else {
      n
    }

  @scala.annotation.tailrec
  def getLargestPrimeFactor(n: Long, i: Long, factors: List[Long]): Long =
    if(i * i <= n) {
      getLargestPrimeFactor(removeFactor(n, i), i + 2, if(n % i == 0) i :: factors else factors)
    } else {
      (n :: factors).max
    }

  println(getLargestPrimeFactor(600851475143L, 3, List.empty))
}
