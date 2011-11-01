import math._

object Problem354 {
	def main(args: Array[String]): Unit = {
		
		var solutions = List[Pair[Int, Int]]()
		
		var y=0;
		
		for {
			n <- 0 to (sqrt(4*y/3)).intValue
			
		}
		
	}
	
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