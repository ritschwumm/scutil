package scutil.lang

import minitest._

@deprecated("use IoResource", "0.203.0")
object UsingBracketTest extends SimpleTestSuite {
	test("Resource.bracket should execute a disposer and a consumer") {
		var tmp	= 0

		val res	=
			Using.unsafeBracket(1)
				{ it => tmp	= it }
				{ it => "test" + it.toString}

		assertEquals(tmp, 1)
		assertEquals(res, "test1")
	}

	test("Resource.bracket should pass on an Exception thrown in the consumer and still execute the disposer") {
		var tmp	= 0
		var err	= null:Exception

		try {
			Using.unsafeBracket[Int,String](1)
				{ it => tmp	= it }
				{ it => sys error "consume failed"; "" }
		}
		catch { case e:Exception =>
			err	= e
		}

		assertEquals(tmp, 1)
		assertEquals(err.getMessage, "consume failed")
	}

	test("Resource.bracket should pass on an Exception thrown in the disposer") {
		var err	= null:Exception

		try {
			Using.unsafeBracket[Int,String](1)
				{ it => sys error "dispose failed" }
				{ it => it.toString }
		}
		catch { case e:Exception =>
			err	= e
		}

		assertEquals(err.getMessage, "dispose failed")
	}

	test("Resource.bracket should add an Exception thrown in the disposer to an exception thrown in the consumer") {
		var err	= null:Exception

		try {
			Using.unsafeBracket[Int,String](1)
				{ it => sys error "dispose failed" }
				{ it => sys error "consume failed"; "" }
		}
		catch { case e:Exception =>
			err	= e
		}

		assertEquals(err.getMessage, "consume failed")
		assertEquals(err.getSuppressed.length, 1)
		assertEquals(err.getSuppressed()(0).getMessage, "dispose failed")
	}
}
