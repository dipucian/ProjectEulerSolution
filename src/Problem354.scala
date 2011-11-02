import math._
import math.{BigInt => Int}

object Problem354 {
	def main(args: Array[String]): Unit = {
		val start = System.currentTimeMillis()
		
		var solutions = List[Pair[Int, Int]]()
		var max = 0
		var count = 0
		var i = 1
		var y = 0
		do {
			y += 1
			//y = i*i
			//if (y > Int.MaxValue/4) throw new Exception("out of range...")
			//i += 1
			solutions = List[Pair[Int, Int]]()
			
			val maxN:Int = (sqrt(4*y/3)).longValue
			for {
				n <- -maxN to maxN		// the range where det >= 0
				root <- asInt(sqrt(det(n, y).doubleValue))
				if ((n & 1) == (root & 1))
			}
			{
				//println("n: "+n+", root: "+root)
				if (root == 0)
				{
					solutions = (n/2, n) :: solutions
				}
				else
				{
					solutions = ((n + root) / 2, n) :: ((n - root) / 2, n) :: solutions
				}
			}
			count = solutions.length
			max = if (count > max) {
				println((System.currentTimeMillis() - start)+"ms "+"\ty: "+y+"\tcount: "+count)
				count
			}
			else max
			
		} while (count != 450)
		
		println(y)
		println(solutions.length)
		println(solutions)
		
		println("runtime: "+(System.currentTimeMillis() - start)+"ms.")
	}
	/*
	private def solveForIntegerM (n:Int, y:Int): Option[Pair[Int, Option[Int]]] = {
		
		val d = det (n, y)
		
		if (d > 0)
		{
			val rootD = asInt(sqrt(d))
			rootD match {
				case Some(int) if ((n & 1) == (int & 1))=> ((n + int)/2, (n - int)/2)
				case _ => None
			}
		}
		
		None
	}
	*/
	private def modf (d:Double): Pair[Int, Double] = {
		val intPart = d.intValue
		(intPart, d - intPart)
	}
	private def isInt (d:Double): Boolean = modf(d)._2 == 0
	private def asInt (d:Double): Option[Int] = {
		modf(d) match {
			case (int, frac) if frac == 0 => Some(int)
			case _ => None
		}
	}
	
	private def fn (m:Int, n:Int): Int = m*m + n*n - m*n
	
	private def det (n:Int, y:Int): Int = 4*y - 3*n*n
}