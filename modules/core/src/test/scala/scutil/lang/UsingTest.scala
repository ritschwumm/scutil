package scutil.lang

import minitest._

object UsingTest extends SimpleTestSuite {
	test("failing to open a second Using inside a first one should dispose the first") {
		var tmp	= ""
		val u	=
			for {
				a	<-	Using.of{ () =>									1 }{ _ => tmp = tmp + "a closed" }
				b	<-	Using.of{ () => sys error "create b failed";	2 }{ _ => tmp = tmp + "b closed" }
			}
			yield ()

		var err	= null:Exception
		try {
			u.run()
		}
		catch { case e:Exception =>
			err = e
		}

		assertEquals(tmp, "a closed")
		assertEquals(err.getMessage, "create b failed")
	}

	test("failing to open a second Using inside a first which fails disposal should return combined exceptions") {
		var tmp	= ""
		val u	=
			for {
				a	<-	Using.of{ () =>									1 }{ _ => sys error "close a failed" }
				b	<-	Using.of{ () => sys error "create b failed";	2 }{ _ => tmp = tmp + "b closed" }
			}
			yield ()

		var err	= null:Exception
		try {
			u.run()
		}
		catch { case e:Exception =>
			err = e
		}

		assertEquals(tmp, "")
		assertEquals(err.getMessage, "create b failed")
		assertEquals(err.getSuppressed.length, 1)
		assertEquals(err.getSuppressed()(0).getMessage, "close a failed")
	}
}
