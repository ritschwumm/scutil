package scutil.math

import org.specs2.mutable._

import scutil.math.implicits._

class MathTest extends Specification {
	"Ordering" should {
		"be contravariant" in {
			trait Numb
			trait Inte extends Numb

			val no:Ordering[Numb]	= null
			val io:Ordering[Inte]	= no.vary[Inte]
			val _ = io

			success
		}
	}

	"functions.modulo" should {
		"work for positive modulos" in {
			((-6 until 6).toVector map { (i:Int) => functions.moduloInt(i, 3) }) mustEqual Vector(0,1,2,0,1,2,0,1,2,0,1,2)
		}
		"work for negative modulos" in {
			((-6 until 6).toVector map { (i:Int) => functions.moduloInt(i, -3) }) mustEqual Vector(0,-2,-1,0,-2,-1,0,-2,-1,0,-2,-1)
		}
	}

	"functions.floorDivInt" should {
		"work for positive value and positive raster" in {
			functions.floorDivInt(11, 2) mustEqual 5
		}
		"work for positive value and negative raster" in {
			functions.floorDivInt(11, -2) mustEqual -6
		}
		"work for negative value and positive raster" in {
			functions.floorDivInt(-11, 2) mustEqual -6
		}
		"work for negative value and negative raster" in {
			functions.floorDivInt(-11, -2) mustEqual 5
		}
		"throw an ArithmeticException for a zero raster" in {
			functions.floorDivInt(1, 0) must throwAn[ArithmeticException]
		}
	}

	"functions.roundDivInt" should {
		"work for positive value and positive raster" in {
			functions.roundDivInt(11, 2) mustEqual 6
		}
		"work for positive value and negative raster" in {
			functions.roundDivInt(11, -2) mustEqual -6
		}
		"work for negative value and positive raster" in {
			functions.roundDivInt(-11, 2) mustEqual -6
		}
		"work for negative value and negative raster" in {
			functions.roundDivInt(-11, -2) mustEqual 6
		}
		"throw an ArithmeticException for a zero raster" in {
			functions.roundDivInt(1, 0) must throwAn[ArithmeticException]
		}
	}

	"functions.ceilDivInt" should {
		"work for positive value and positive raster" in {
			functions.ceilDivInt(11, 2) mustEqual 6
		}
		"work for positive value and negative raster" in {
			functions.ceilDivInt(11, -2) mustEqual -5
		}
		"work for negative value and positive raster" in {
			functions.ceilDivInt(-11, 2) mustEqual -5
		}
		"work for negative value and negative raster" in {
			functions.ceilDivInt(-11, -2) mustEqual 6
		}
		"throw an ArithmeticException for a zero raster" in {
			functions.ceilDivInt(1, 0) must throwAn[ArithmeticException]
		}
	}
}
