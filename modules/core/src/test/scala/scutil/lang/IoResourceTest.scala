package scutil.lang

import minitest._

object IoResourceTest extends SimpleTestSuite {
	test("failing to open a second IoResource inside a first one should dispose the first") {
		var tmp	= ""
		val u	=
			for {
				a	<-	IoResource.unsafe.disposing{								1 }{ _ => tmp = tmp + "a closed" }
				b	<-	IoResource.unsafe.disposing{ sys error "create b failed";	2 }{ _ => tmp = tmp + "b closed" }
			}
			yield ()

		var err	= null:Exception
		try {
			u.useVoid().unsafeRun()
		}
		catch { case e:Exception =>
			err = e
		}

		assertEquals(tmp, "a closed")
		assertEquals(err.getMessage, "create b failed")
	}

	test("failing to open a second IoResource inside a first which fails disposal should return combined exceptions") {
		var tmp	= ""
		val u	=
			for {
				a	<-	IoResource.unsafe.disposing{ 								1 }{ _ => sys error "close a failed" }
				b	<-	IoResource.unsafe.disposing{ sys error "create b failed";	2 }{ _ => tmp = tmp + "b closed" }
			}
			yield ()

		var err	= null:Exception
		try {
			u.useVoid().unsafeRun()
		}
		catch { case e:Exception =>
			err = e
		}

		assertEquals(tmp, "")
		assertEquals(err.getMessage, "create b failed")
		assertEquals(err.getSuppressed.length, 1)
		assertEquals(err.getSuppressed()(0).getMessage, "close a failed")
	}

	//------------------------------------------------------------------------------

	test("IoResource.unsafeUse should execute a disposer and a consumer") {
		var tmp	= 0

		val res	=
			IoResource.unsafe.disposing(1){ it => tmp	= it }
			.unsafeUse{ it => "test" + it.toString}
			.unsafeRun()

		assertEquals(tmp, 1)
		assertEquals(res, "test1")
	}

	test("IoResource.unsafeUse should pass on an Exception thrown in the consumer and still execute the disposer") {
		var tmp	= 0
		var err	= null:Exception

		try {
			IoResource.unsafe.disposing(1){ it => tmp	= it }
			.unsafeUse{ it => sys error "consume failed"; "" }
			.unsafeRun()
		}
		catch { case e:Exception =>
			err	= e
		}

		assertEquals(tmp, 1)
		assertEquals(err.getMessage, "consume failed")
	}

	test("IoResource.unsafeUse should pass on an Exception thrown in the disposer") {
		var err	= null:Exception

		try {
			IoResource.unsafe.disposing(1){ it => sys error "dispose failed" }
			.unsafeUse{ it => it.toString }
			.unsafeRun()
		}
		catch { case e:Exception =>
			err	= e
		}

		assertEquals(err.getMessage, "dispose failed")
	}

	test("IoResource.unsafeUse should add an Exception thrown in the disposer to an exception thrown in the consumer") {
		var err	= null:Exception

		try {
			IoResource.unsafe.disposing(1){ it => sys error "dispose failed" }
			.unsafeUse{ it => sys error "consume failed"; "" }
			.unsafeRun()
		}
		catch { case e:Exception =>
			err	= e
		}

		assertEquals(err.getMessage, "consume failed")
		assertEquals(err.getSuppressed.length, 1)
		assertEquals(err.getSuppressed()(0).getMessage, "dispose failed")
	}
}
