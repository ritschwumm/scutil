package scutil

import org.specs._
import org.specs.matcher._

import scutil._
		
class Base64Test extends Specification {
	//val hexDump	= new HexDump(60)
	val	possible = {
		val	out	= new Array[Byte](256)
		for (i <- 0 until 256) out(i) = i.toByte
		out
	}
	
	"Base64" should {
		"handle roundtriping 0 bytes at all" in {
			val bytes	= Array[Byte]()
			val round	= Base64 unapply (Base64 apply bytes)
			round must beSome[Array[Byte]]
		}
		"correctly roundtrip 0 bytes" in {
			val bytes	= Array[Byte]()
			val round	= Base64 unapply (Base64 apply bytes)
			round.get must haveTheSameElementsAs(bytes)
		}
		
		"handle roundtriping 1 bytes at all" in {
			val bytes	= Array[Byte](0)
			val round	= Base64 unapply (Base64 apply bytes)
			round must beSome[Array[Byte]]
		}
		"correctly roundtrip 1 bytes" in {
			val bytes	= Array[Byte](0)
			val round	= Base64 unapply (Base64 apply bytes)
			round.get must haveTheSameElementsAs(bytes)
		}
		
		"handle roundtriping 2 bytes at all" in {
			val bytes	= Array[Byte](1,2)
			val round	= Base64 unapply (Base64 apply bytes)
			round must beSome[Array[Byte]]
		}
		"correctly roundtrip 2 bytes" in {
			val bytes	= Array[Byte](1,2)
			val round	= Base64 unapply (Base64 apply bytes)
			round.get must haveTheSameElementsAs(bytes)
		}
		
		"handle roundtriping 3 bytes at all" in {
			val bytes	= Array[Byte](3,4,5)
			val round	= Base64 unapply (Base64 apply bytes)
			round must beSome[Array[Byte]]
		}
		"correctly roundtrip 2 bytes" in {
			val bytes	= Array[Byte](3,4,5)
			val round	= Base64 unapply (Base64 apply bytes)
			round.get must haveTheSameElementsAs(bytes)
		}  
		
		"handle roundtriping 4 bytes at all" in {
			val bytes	= Array[Byte](6,7,8,9)
			val round	= Base64 unapply (Base64 apply bytes)
			round must beSome[Array[Byte]]
		}
		"correctly roundtrip 2 bytes" in {
			val bytes	= Array[Byte](6,7,8,9)
			val round	= Base64 unapply (Base64 apply bytes)
			round.get must haveTheSameElementsAs(bytes)
		}    
		
		"handle roundtriping every possible byte at all" in {
			val bytes	= possible
			val round	= Base64 unapply (Base64 apply bytes)
			round must beSome[Array[Byte]]
		}
		"correctly roundtrip every possible byte bytes" in {
			val bytes	= possible
			val round	= Base64 unapply (Base64 apply bytes)
			round.get must haveTheSameElementsAs(bytes)
		}    
	}
}
