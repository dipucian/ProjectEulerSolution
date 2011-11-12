package projecteuler.problems

import util.prime.PrimeUtil._

object Problem3 {
	def main(args: Array[String]): Unit = {
		val n: Long = 600851475143L;
		println("What is the largest prime factor of the number " + n + " ?")
		
		time {
			println("Answer: "+largestPrimeFactor(n))
		}
	}

	private def largestPrimeFactor(n: Long): Long = {
		var t = n
		for (p <- allPrimes) {
			while (t % p == 0) {
				t = t / p
			}
			if (t == 1) return p
		}
		2
	}
}
