package scutil.lang.tc

import minitest._

import scutil.core.implicits._
import scutil.lang._

object ApplicativeTest extends SimpleTestSuite {
	test("map2 in the zip order when zippy") {
		assertEquals(
			Applicative[Option].map2(Some(1), Some(2))(_ -> _),
			(Some(1) zip Some(2))
		)
	}

	test("map2 in the same order as flatMap for Vector") {
		val as	= Vector(1,2)
		val bs	= Vector(3,4)

		// 3,4,6,8
		val x	=
			as flatMap { a =>
				bs map { b =>
					a*b
				}
			}
		assertEquals(
			x,
			Vector(3,4,6,8)
		)

		val y	= (as map2 bs)(_*_)
		assertEquals(y, x)
	}

	test("map2 in the same order as flatMap for Nes") {
		val as	= Nes.of(1,2)
		val bs	= Nes.of(3,4)

		// 3,4,6,8
		val x	=
			as flatMap { a =>
				bs map { b =>
					a*b
				}
			}
		assertEquals(
			x,
			Nes.of(3,4,6,8)
		)

		val y	= (as map2 bs)(_*_)
		assertEquals(y, x)
	}

	test("have ap do the same thing for native and instance") {
		val f1:Int=>Int	= _-1
		val f2:Int=>Int	= _+1

		val as	= Vector(3,4)
		val bs	= Vector(f1, f2)

		// function effect first
		val x	= bs ap as
		assertEquals(
			x,
			Vector(2,3,4,5)
		)

		val y	= Applicative[Vector].ap(bs)(as)
		assertEquals(y, x)
	}

	test("do the function effect first") {
		val f:Either[String,Int=>Int]	= Left("function")
		val v:Either[String,Int]		= Left("value")
		assertEquals(
			Applicative[Either[String,*]].ap(f)(v),
			Left("function")
		)
	}
}
