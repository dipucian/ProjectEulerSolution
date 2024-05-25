package projecteuler.problems

import util.prime.PrimeUtil.primeFactorize

object Problem21 {
  def main(args: Array[String]): Unit = {
    time {
      val amicableNumbers = (2 until 10000).filter { n =>
        val m = sumOfProperDivisors(n)
        m != 1 && m != n && sumOfProperDivisors(m) == n
      }
      println(amicableNumbers.sum)
    }
  }

  def sumOfProperDivisors(n: Long): Long = {
    val factors = primeFactorize(n)
    println(s"factors of $n: $factors")
    factors.foldLeft(1L) {
      case (acc, (p, m)) => acc * (math.pow(p, m + 1).toInt - 1) / (p - 1)
    } - n
  }
}
