package scutil.lang

import minitest._

sealed trait PrismSuper
case object			PrismObject			extends PrismSuper
final case class	PrismClass(a:Int)	extends PrismSuper

object PrismGenTest extends SimpleTestSuite {
	test("PrismGen should get a case object") {
		val prism					= Prism.Gen[PrismSuper,PrismObject.type]
		val sub:PrismObject.type	= PrismObject
		val sup:PrismSuper			= sub
		assertEquals(
			prism get sup,
			Some(sub)
		)
	}

	test("PrismGen should put a case object") {
		val prism					= Prism.Gen[PrismSuper,PrismObject.type]
		val sub:PrismObject.type	= PrismObject
		val sup:PrismSuper			= sub
		assertEquals(
			prism set sub,
			sup
		)
	}

	test("PrismGen should get a case class") {
		val prism			= Prism.Gen[PrismSuper,PrismClass]
		val sub:PrismClass	= PrismClass(1)
		val sup:PrismSuper	= sub
		assertEquals(
			prism get sup,
			Some(sub)
		)
	}
	test("PrismGen should put a case class") {
		val prism			= Prism.Gen[PrismSuper,PrismClass]
		val sub:PrismClass	= PrismClass(1)
		val sup:PrismSuper	= sub
		assertEquals(
			prism set sub,
			sup
		)
	}
}
