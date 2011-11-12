package projecteuler.problems

object Problem9 {

	def main(args: Array[String]): Unit = {
		val problemStatement = "A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^2 + b^2 = c^2" +
				"\nThere exists exactly one Pythagorean triplet for which a + b + c = 1000." +
				"\nFind the product abc."
		println(problemStatement)
		
		val triplet = find(1000)
		println("Answer: "+ (triplet._1 * triplet._2 * triplet._3))
	}
	
	def find(sum:Int):(Int, Int, Int) = {
		for {
			c <- sum/3+2 to sum-3
			c2 = c*c
			r = sum - c
			a <- 1 to c/2
			b = r - a
			if (a*a + b*b == c2)
		}
		return (a, b, c)
		throw new Exception("Not found")
	}

}