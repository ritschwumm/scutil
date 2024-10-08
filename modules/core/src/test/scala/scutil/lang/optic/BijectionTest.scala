package scutil.lang

import minitest.*

@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
object BijectionTest extends SimpleTestSuite {
	case object CaseObj
	final case class Unary(a:Int)
	final case class Binary(a:Int, b:Short)
	final case class Container(x:Unary)
	final case class UnaryOption(a:Option[Int])

	trait Fooz[T]
	final case class Parametrized1[A](x:Fooz[A])
	final case class Parametrized2[A,B](x:Fooz[A], y:Fooz[B])

	@SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
	object Wrapper {
		final case class Inner(t:Int)
	}

	//-----------------------------------------------------------------------------

	test("Bijection should work with case objects") {
		val value		= CaseObj
		val	bijection	= Bijection.zero[CaseObj.type]
		assertEquals(
			bijection.get(value),
			(())
		)
	}

	test("Bijection should work with unary case classes") {
		val value		= Unary(1)
		val	bijection	= Bijection.one[Unary]
		assertEquals(
			bijection.get(value),
			1
		)
	}

	test("Bijection should work with binary case classes") {
		val value		= Binary(1,2)
		val	bijection	= Bijection.more[Binary]
		assertEquals[(Int,Short)](
			bijection.get(value),
			(1,2)
		)
	}

	test("Bijection should work with nested case classes") {
		val value		= Container(Unary(1))
		val	bijection	= Bijection.one[Container]
		assertEquals(
			bijection.get(value),
			Unary(1)
		)
	}

	test("Bijection should work with unary case classes where the argument is a Product") {
		val value		= UnaryOption(Some(1))
		val	bijection	= Bijection.one[UnaryOption]
		assertEquals(
			bijection.get(value),
			Some(1)
		)
	}

	test("Bijection should work with case classes with one type parameter") {
		val _ = Bijection.one[Parametrized1[Int]]
		assertEquals(
			1,
			1
		)
	}

	test("Bijection should work with case classes with multiple type parameters") {
		val _ = Bijection.more[Parametrized2[Int,String]]
		assertEquals(
			1,
			1
		)
	}
}
