package scutil.lang.tc

import org.specs2.mutable._

import scutil.core.implicits._
import scutil.lang._

class ApplicativeTest extends Specification {
	"applicative" should {
		"map2 in the zip order when zippy" in {
			Applicative[Option].map2(Some(1), Some(2))(_ -> _) mustEqual (Some(1) zip Some(2))
		}

		"map2 in the same order as flatMap for Vector" in {
			val as	= Vector(1,2)
			val bs	= Vector(3,4)

			// 3,4,6,8
			val x	=
					as flatMap { a =>
						bs map { b =>
							a*b
						}
					}
			x mustEqual Vector(3,4,6,8)

			val y	= (as map2 bs)(_*_)
			x mustEqual y
		}

		"map2 in the same order as flatMap for Nes" in {
			val as	= Nes.of(1,2)
			val bs	= Nes.of(3,4)

			// 3,4,6,8
			val x	=
					as flatMap { a =>
						bs map { b =>
							a*b
						}
					}
			x mustEqual Nes.of(3,4,6,8)

			val y	= (as map2 bs)(_*_)
			x mustEqual y
		}

		"have ap do the same thing for native and instance" in {
			val f1:Int=>Int	= _-1
			val f2:Int=>Int	= _+1

			val as	= Vector(3,4)
			val bs	= Vector(f1, f2)

			// function effect first
			val x	= as pa bs
			x mustEqual Vector(2,3,4,5)

			val y	= (Applicative[Vector] ap as)(bs)
			x mustEqual y
		}

		"do the function effect first" in {
			val f:Either[String,Int=>Int]	= Left("function")
			val v:Either[String,Int]		= Left("value")
			(Applicative[Either[String,*]] ap v)(f) mustEqual Left("function")
		}
	}
}
