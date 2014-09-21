package scutil.color

import org.specs2.mutable._

import scutil.color.implicits._

class ColorTest extends Specification {
	"HexContext" should {
		"decode a valid color" in {
			rgb"000000" == RGB(0,0,0)
		}
		// "fail at compile time for invalid colors" in {
		// 	rgb"xxxxxx" == RGB(0,0,0)
		// }
	}
}
