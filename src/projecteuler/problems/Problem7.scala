package projecteuler.problems

import util.prime.PrimeUtil.allPrimes

object Problem7 {
	def main(args: Array[String]): Unit = {
		println("What is the 10 001st prime number?")
		
		time {
			println("Answer: "+allPrimes(10000))
		}
	}
}
