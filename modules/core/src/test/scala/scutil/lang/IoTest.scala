package scutil.lang

import org.specs2.mutable._

class IoTest extends Specification {
	"Io" should {
		"flatMap long chains without stack overflow" in {
			//val before = System.currentTimeMillis()
			var base	= Io pure 0
			for (i <- 1 to 100000) {
				base	= base flatMap (_ => Io pure i)
				base	= base map (_ => i)
			}
			val out	= base.unsafeRun()
			//val after = System.currentTimeMillis()
			//println(s"### needed ${after - before} ms")
			out mustEqual 100000
		}
	}
}
