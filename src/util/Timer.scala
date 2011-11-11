package util

object Timer {
	def time(fn: => Unit) = {
		var start = System.currentTimeMillis
		fn
		println((System.currentTimeMillis - start)+"ms.")
	}
}