package scutil.math

import minitest.*

import scutil.math.implicits.*

object MathTest extends SimpleTestSuite {
	test("Ordering should be contravariant") {
		trait Numb
		trait Inte extends Numb

		val no:Ordering[Numb]	= null
		val io:Ordering[Inte]	= no.vary[Inte]
		val _ = io

		()
	}

	//------------------------------------------------------------------------------

	test("functions.modulo should work for positive modulos") {
		assertEquals(
			((-6 until 6).toVector map { (i:Int) => functions.moduloInt(i, 3) }),
			Vector(0,1,2,0,1,2,0,1,2,0,1,2)
		)
	}

	test("functions.modulo should work for negative modulos") {
		assertEquals(
			((-6 until 6).toVector map { (i:Int) => functions.moduloInt(i, -3) }),
			Vector(0,-2,-1,0,-2,-1,0,-2,-1,0,-2,-1)
		)
	}

	//------------------------------------------------------------------------------

	test("functions.floorDivInt should work for positive value and positive raster") {
		assertEquals(
			functions.floorDivInt(11, 2),
			5
		)
	}

	test("functions.floorDivInt should work for positive value and negative raster") {
		assertEquals(
			functions.floorDivInt(11, -2),
			-6
		)
	}

	test("functions.floorDivInt should work for negative value and positive raster") {
		assertEquals(
			functions.floorDivInt(-11, 2),
			-6
		)
	}

	test("functions.floorDivInt should work for negative value and negative raster") {
		assertEquals(
			functions.floorDivInt(-11, -2),
			5
		)
	}

	test("functions.floorDivInt should throw an ArithmeticException for a zero raster") {
		intercept[ArithmeticException] {
			functions.floorDivInt(1, 0)
		}
		()
	}

	//------------------------------------------------------------------------------

	test("functions.roundDivInt should work for positive value and positive raster") {
		assertEquals(
			functions.roundDivInt(11, 2),
			6
		)
	}

	test("functions.roundDivInt should work for positive value and negative raster") {
		assertEquals(
			functions.roundDivInt(11, -2),
			-6
		)
	}

	test("functions.roundDivInt should work for negative value and positive raster") {
		assertEquals(
			functions.roundDivInt(-11, 2),
			-6
		)
	}

	test("functions.roundDivInt should work for negative value and negative raster") {
		assertEquals(
			functions.roundDivInt(-11, -2),
			6
		)
	}

	test("functions.roundDivInt should throw an ArithmeticException for a zero raster") {
		intercept[ArithmeticException] {
			functions.roundDivInt(1, 0)
		}
		()
	}

	//------------------------------------------------------------------------------

	test("functions.ceilDivInt should work for positive value and positive raster") {
		assertEquals(
			functions.ceilDivInt(11, 2),
			6
		)
	}

	test("functions.ceilDivInt should work for positive value and negative raster") {
		assertEquals(
			functions.ceilDivInt(11, -2),
			-5
		)
	}

	test("functions.ceilDivInt should work for negative value and positive raster") {
		assertEquals(
			functions.ceilDivInt(-11, 2),
			-5
		)
	}

	test("functions.ceilDivInt should work for negative value and negative raster") {
		assertEquals(
			functions.ceilDivInt(-11, -2),
			6
		)
	}

	test("functions.ceilDivInt should throw an ArithmeticException for a zero raster") {
		intercept[ArithmeticException] {
			functions.ceilDivInt(1, 0)
		}
		()
	}
}
