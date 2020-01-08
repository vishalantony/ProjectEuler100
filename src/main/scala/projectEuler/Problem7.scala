package projectEuler

object Problem7 extends App {

  val LIMIT = 10001

  @scala.annotation.tailrec
  def sieve(n: Int, limit: Int, l: Array[Boolean])(offset: Int): Array[Boolean] = {
    if (n * n > limit) {
      l
    } else if (l(n - offset)) {
      sieve(n + 1, limit, l)(offset)
    } else {
      for {
        i <- n * n to limit by n
      } yield l(i - offset) = true
      sieve(n + 1, limit, l)(offset)
    }
  }

  def getPrimes(from: Int, to: Int): Array[Int] = {
    val primes = Array.fill(to - from + 1)(false)
    sieve(from, to, primes)(from).zip(from to to).filterNot(_._1).map(_._2)
  }

  @scala.annotation.tailrec
  def findNthPrime(n: Int)(start: Int, stop: Int, step: Int): Int = {
    val primes = getPrimes(start, stop)
    if(primes.length >= n) {
      primes(n-1)
    } else {
      findNthPrime(n)(start, stop+step, step)
    }
  }

  println(findNthPrime(LIMIT)(2, 100000, 10000))
}
