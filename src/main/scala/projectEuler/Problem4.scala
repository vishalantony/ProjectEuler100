package projectEuler

object Problem4 extends App {
  def isPalindrome(s: String): Boolean = {
    val l = s.length
    (for (i <- 0 until l / 2) yield s(i) == s(l - i - 1)).forall(x => x)
  }

  def isPalindrome(n: Int): Boolean = isPalindrome(s"$n")

  def findLargestPalindrome: Int = {
    var largestPalindrome = 0
    for {
      i <- 999 to 100 by -1
      j <- i to 100 by -1
      prod = i * j
      if isPalindrome(prod) && prod > largestPalindrome
      _  = largestPalindrome = prod
    } ()

    largestPalindrome
  }

  println(findLargestPalindrome)
}
