package scutil.lang

import org.specs2.mutable._

import scutil.lang.implicits._

class ShowInterpolatorTest extends Specification {
	import scutil.lang.tc._
	implicit def OptionShow[T:Show]:Show[Option[T]]	=
			Show instance {
				case Some(x)	=> "some: " + (Show doit x)
				case None		=> "none"
			}
			
	"show interpolator" should {
		"do an empty string" in {
			show"""""" mustEqual ""
		}
		"do a single string" in {
			show"""a""" mustEqual "a"
		}
		"do a single value" in {
			val a = "1"
			show"""$a""" mustEqual "1"
		}
		"do a string and a value" in {
			val a = "1"
			show"""${a}test""" mustEqual "1test"
		}
		"do a value and a string" in {
			val a = "1"
			show"""test${a}""" mustEqual "test1"
		}
		"work with multiple values and types" in {
			val a = "1"
			val b = 2
			val c = true
			show"""aaa${a}bbb${b}ccc${c}""" mustEqual "aaa1bbb2ccctrue"
		}
		
		"work with custom instances" in {
			val o:Option[Int]	= Some(1)
			show"""$o""" mustEqual "some: 1"
		}
		/*
		"work with inheritnce" in {
			val o:Some[Int]	= Some(1)
			show"""$o""" mustEqual "some: 1"
		}
		*/
	}
}