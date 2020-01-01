package scutil.lang

import org.specs2.mutable._

import scutil.lang.tc._

class ByteStringTest extends Specification {
	"ByteString" should {
		"have a working monoid" in {
			val xs	= Vector(ByteString fromUtf8String "a", ByteString fromUtf8String "b")
			val out	= (xs foldLeft Monoid[ByteString].empty)(Monoid[ByteString].concat)

			out mustEqual (ByteString fromUtf8String "ab")
		}
	}
}
