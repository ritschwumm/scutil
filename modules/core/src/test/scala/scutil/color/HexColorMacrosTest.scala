package scutil.color

import org.specs2.mutable._

import scutil.color.implicits._

class HexColorMacrosTest extends Specification {
	"HexColorMacros" should {
		"decode a valid color" in {
			rgb"000000" mustEqual RGB(0,0,0)
		}
		/*
		"fail at compile time for invalid colors" in {
			rgb"xxxxxx" mustEqual RGB(0,0,0)
		}
		*/
	}
}
