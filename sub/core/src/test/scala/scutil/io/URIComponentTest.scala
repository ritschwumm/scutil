package scutil.io

import org.specs2.mutable._

class URIComponentTest extends Specification {
	"URIComponent" should {
		"roundtrip all usual chars" in {
			val str	= 0 until 256 map { _.toChar } mkString ""
			val	enc	= URIComponent encode str
			val	dec	= URIComponent decode enc
			dec mustEqual str
		}   
		"encode plus as %2B" in {
			URIComponent encode "+" mustEqual "%2B"
		}
		"encode blank as %20" in {
			URIComponent encode " " mustEqual "%20"
		}
		"decode plus as plus" in {
			URIComponent decode "+" mustEqual "+"
		}
		
		val interestingRaw	= "~!@#$%^&*(){}[]=:/,;?+'\"\\"
		val interestingCode	= "~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B'%22%5C"
		
		"encode interesting chars just like encodeURIComponent" in {
			URIComponent encode interestingRaw mustEqual interestingCode
		}
		
		"decode interesting chars just like decodeURIComponent" in {
			URIComponent decode interestingCode mustEqual interestingRaw
		}
		
		"encode german umlauts" in {
			URIComponent encode "äöü" mustEqual "%C3%A4%C3%B6%C3%BC"
		}
		
		"decode german umlauts" in {
			URIComponent decode "%C3%A4%C3%B6%C3%BC" mustEqual "äöü"
		}
		
		/*
		import javax.script.ScriptEngineManager
		
		"encode everything just like encodeURIComponent" in {
			val engine	= new ScriptEngineManager getEngineByName "JavaScript"
			val str	= 0 until 256 map { _.toChar } mkString ""
			val s1	= URIComponent encode (str, utf_8)
			engine put ("str", str)
			val s2	= engine eval """encodeURIComponent(str)"""
			s1 mustEqual s2
		}
		
		"decode everything just like encodeURIComponent" in {
			val engine	= new ScriptEngineManager getEngineByName "JavaScript"
			val str	= 0 until 256 map { _.toChar } mkString ""
			engine put ("str", str)
			val s1	= (engine eval """encodeURIComponent(str)""").asInstanceOf[String]
			val s2	= URIComponent decode (s1, utf_8)
			s2 mustEqual str
		}
		*/
	}
}
