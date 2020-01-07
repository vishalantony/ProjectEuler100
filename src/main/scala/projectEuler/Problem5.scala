package projectEuler

object Problem5 extends App {
  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) {
      a
    } else {
      gcd(b, a % b)
    }

  def lcm(a: Long, b: Long): Long =
    a * b / gcd(a, b)

  def getLCMTill(n: Int): Long =
    (1 to n).foldLeft(1L)((acc, i) => lcm(acc, i.toLong))

  println(getLCMTill(20))


  /** * Another Solution ***/

  // returns all the primes till `limit` starting from n as Array of size limit
  // if the index `i` of the boolean array is false, then i is prime.
  @scala.annotation.tailrec
  def sieve(n: Int, limit: Int, l: Array[Boolean]): Array[Boolean] =
    if (n * n > limit) {
      l
    } else {
      for {
        i <- n * n to limit by n
      } yield (l(i) = true)
      sieve(n + 1, limit, l)
    }

  // Gets all the prime numbers till number n
  def getPrimesTill(n: Int): Array[Int] = {
    val isPrime = Array.fill(n + 1)(false)
    isPrime(0) = true
    isPrime(1) = true
    sieve(2, n, isPrime).zipWithIndex.filter(!_._1).map(_._2)
  }

  // Tells you how many times p divides n. i.e., floor(Log_p_(n)) (floor of log of n base p)
  @scala.annotation.tailrec
  def goesInNTimes(n: Int, p: Int, times: Int = 0): Int = {
    if (n % p != 0) {
      times
    } else {
      goesInNTimes(n / p, p, times + 1)
    }
  }

  // gives you the `e`th power of `n`
  def power(n: Int, e: Int): Long = {
    if (e == 0) {
      1
    } else if (e % 2 == 0) {
      val p = power(n, e / 2)
      p * p
    } else {
      val p = power(n, e / 2)
      p * p * n
    }
  }

  def logarithm(base: Int, number: Int): Int = {
    @scala.annotation.tailrec
    def raise(b: Int, p: Int = 0): Int =
      if (b > number) {
        p - 1
      } else if (b == number) {
        p
      } else {
        raise(b * base, p + 1)
      }

    raise(1)
  }

  // For each prime number `p` from 2 to `n`, find the logarithm of `n` to the base `p`
  def getLeastPrimesTable(n: Int): Map[Int, Int] =
    getPrimesTill(n).foldLeft(Map.empty[Int, Int])((acc, p) => acc + (p -> logarithm(p, n)))

  // After finding the primes table, this function multiplies all the primes.
  def getLCMOfTill(n: Int): Long =
    getLeastPrimesTable(n).toList.foldLeft(1L)((acc, fac) => {
      acc * power(fac._1, fac._2)
    })

  println(getLCMOfTill(20))
}
