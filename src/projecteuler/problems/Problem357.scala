package projecteuler.problems

object Problem357 {

	def main(args: Array[String]): Unit = {

		val problemStatement = "Find the sum of all positive integers n not exceeding 100 000 000" +
			"\nsuch that for every divisor d of n, d+n/d is prime."
		println(problemStatement)

		import util.prime.PrimeUtil._
		
		time {
			var sum = 1 + 2 + 6L
			for {
				n <- 10 until 100000000 by 4
				if (isPrimeGenerator(n))
			} sum += n

			println("Answer: " + sum)
		}
	}

}