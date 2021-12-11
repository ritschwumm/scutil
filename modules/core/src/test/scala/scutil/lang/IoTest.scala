package scutil.lang

import minitest.*

object IoTest extends SimpleTestSuite {
	test("Io should flatMap long chains without stack overflow") {
		//val before = System.currentTimeMillis()
		var base	= Io pure 0
		for (i <- 1 to 100000) {
			base	= base flatMap (_ => Io pure i)
			base	= base map (_ => i)
		}
		val out	= base.unsafeRun()
		//val after = System.currentTimeMillis()
		//println(s"### needed ${after - before} ms")
		assertEquals(
			out,
			100000
		)
	}
}
