package scutil.lang

import minitest.*

object IoDisposerTest extends SimpleTestSuite {
	test("a combined Disposer should execute both actions in order") {
		var tmp	= ""
		val a	= IoDisposer delay { tmp = tmp + "a" }
		val b	= IoDisposer delay { tmp = tmp + "b" }
		val c	= a combine b
		c.unsafeRun()

		assertEquals(tmp, "ab")
	}

	test("in an exception thrown in the first of a combined Disposer should be work") {
		var tmp	= 0
		var err	= null:Exception
		val a	= IoDisposer delay { sys error "a failed" }
		val b	= IoDisposer delay { tmp = 2 }
		val c	= a combine b
		try {
			c.unsafeRun()
		}
		catch { case e:Exception =>
			err = e
		}
		assertEquals(tmp, 2)
		assertEquals(err.getMessage, "a failed")
	}

	test("in an exception thrown in the second of a combined Disposer should be work") {
		var tmp	= 0
		var err	= null:Exception
		val a	= IoDisposer delay { tmp = 1 }
		val b	= IoDisposer delay { sys error "b failed" }
		val c	= a combine b
		try {
			c.unsafeRun()
		}
		catch { case e:Exception =>
			err = e
		}
		assertEquals(tmp, 1)
		assertEquals(err.getMessage, "b failed")
	}

	test("in an exception thrown in both a combined Disposer should be work") {
		var err	= null:Exception
		val a	= IoDisposer delay { sys error "a failed" }
		val b	= IoDisposer delay { sys error "b failed" }
		val c	= a combine b
		try {
			c.unsafeRun()
		}
		catch { case e:Exception =>
			err = e
		}
		assertEquals(err.getMessage, "a failed")
		assertEquals(err.getSuppressed.length, 1)
		assertEquals(err.getSuppressed()(0).getMessage, "b failed")
	}
}
