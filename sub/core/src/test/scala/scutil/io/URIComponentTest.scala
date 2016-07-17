package scutil.io

import org.specs2.mutable._

import scutil.lang._

class URIComponentTest extends Specification {
	"URIComponent" should {
		"roundtrip all usual chars (low)" in {
			val str	= '\u0000' to '\ud7ff' map { _.toChar } mkString ""
			val	enc	= URIComponent.utf_8 encode str
			val	dec	= URIComponent.utf_8 decode enc
			dec mustEqual Win(str)
		}
		"roundtrip all usual chars (high)" in {
			val str	= '\ue000' to '\uffff' map { _.toChar } mkString ""
			val	enc	= URIComponent.utf_8 encode str
			val	dec	= URIComponent.utf_8 decode enc
			dec mustEqual Win(str)
		}
		"roundtrip chars outside the 16 bit range" in {
			val str	= Character toChars Integer.parseInt("00010400", 16) mkString ""
			val	enc	= URIComponent.utf_8 encode str
			val	dec	= URIComponent.utf_8 decode enc
			dec mustEqual Win(str)
		}
		
		"encode plus as %2B" in {
			URIComponent.utf_8 encode "+" mustEqual "%2B"
		}
		"encode blank as %20" in {
			URIComponent.utf_8 encode " " mustEqual "%20"
		}
		"decode plus as plus" in {
			URIComponent.utf_8 decode "+" mustEqual Win("+")
		}
		
		val interestingRaw	= "~!@#$%^&*(){}[]=:/,;?+'\"\\"
		val interestingCode	= "~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B'%22%5C"
		
		"encode interesting chars just like encodeURIComponent" in {
			URIComponent.utf_8 encode interestingRaw mustEqual interestingCode
		}
		
		"decode interesting chars just like decodeURIComponent" in {
			URIComponent.utf_8 decode interestingCode mustEqual Win(interestingRaw)
		}
		
		"encode german umlauts" in {
			URIComponent.utf_8 encode "äöü" mustEqual "%C3%A4%C3%B6%C3%BC"
		}
		
		"decode german umlauts" in {
			URIComponent.utf_8 decode "%C3%A4%C3%B6%C3%BC" mustEqual Win("äöü")
		}
		
		"fail with invalid % sequences (1)" in {
			URIComponent.utf_8 decode "%" mustEqual Fail(URIComponentInvalid((1)))
		}
		
		"fail with invalid % sequences (2)" in {
			URIComponent.utf_8 decode " %" mustEqual Fail(URIComponentInvalid((2)))
		}
		
		"fail with invalid % sequences (3)" in {
			URIComponent.utf_8 decode "%x" mustEqual Fail(URIComponentInvalid((1)))
		}
		
		"fail with invalid % sequences (4)" in {
			URIComponent.utf_8 decode "%1x" mustEqual Fail(URIComponentInvalid((2)))
		}
		
		"fail with invalid % sequences (5)" in {
			URIComponent.utf_8 decode "%%" mustEqual Fail(URIComponentInvalid((1)))
		}
		
		/*
		import javax.script.ScriptEngineManager
		
		"encode everything just like encodeURIComponent" in {
			val engine	= new ScriptEngineManager getEngineByName "JavaScript"
			val str	= 0 until 256 map { _.toChar } mkString ""
			val s1	= URIComponent.utf_8 encode (str, utf_8)
			engine put ("str", str)
			val s2	= engine eval """encodeURIComponent(str)"""
			s1 mustEqual s2
		}
		
		"decode everything just like encodeURIComponent" in {
			val engine	= new ScriptEngineManager getEngineByName "JavaScript"
			val str	= 0 until 256 map { _.toChar } mkString ""
			engine put ("str", str)
			val s1	= (engine eval """encodeURIComponent(str)""").asInstanceOf[String]
			val s2	= URIComponent.utf_8 decode (s1, utf_8)
			s2 mustEqual str
		}
		*/
	}
}
