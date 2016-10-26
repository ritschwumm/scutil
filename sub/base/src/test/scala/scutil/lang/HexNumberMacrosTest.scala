package scutil.lang

import org.specs2.mutable._

import scutil.lang.implicits._

class HexNumberMacrosTest extends Specification {
	"HexNumberMacros" should {
		"decode 00" in {
			val it	= byte"00"
			typed[Byte](it)
			it mustEqual 0.toByte
		}
		"decode 7f" in {
			val it	= byte"7f"
			typed[Byte](it)
			it mustEqual 127.toByte
		}
		"decode 80" in {
			val it	= byte"80"
			typed[Byte](it)
			it mustEqual 128.toByte
		}
		"decode ff" in {
			val it	= byte"ff"
			typed[Byte](it)
			it mustEqual 255.toByte
		}
		
		"decode 0000" in {
			val it	= short"0000"
			typed[Short](it)
			it mustEqual 0.toShort
		}
		"decode 7fff" in {
			val it	= short"7fff"
			typed[Short](it)
			it mustEqual 32767.toShort
		}
		"decode 8000" in {
			val it	= short"8000"
			typed[Short](it)
			it mustEqual 32768.toShort
		}
		"decode ffff" in {
			val it	= short"ffff"
			typed[Short](it)
			it mustEqual 65535.toShort
		}
		
		"decode 00000000" in {
			val it	= int"00000000"
			typed[Int](it)
			it mustEqual 0.toInt
		}
		"decode 7fffffff" in {
			val it	= int"7fffffff"
			typed[Int](it)
			it mustEqual 2147483647.toInt
		}
		"decode 80000000" in {
			val it	= int"80000000"
			typed[Int](it)
			it mustEqual -2147483648.toInt
		}
		"decode ffffffff" in {
			val it	= int"ffffffff"
			typed[Int](it)
			it mustEqual -1.toInt
		}
		
		"decode 0000000000000000" in {
			val it	= long"0000000000000000"
			typed[Long](it)
			it mustEqual 0.toInt
		}
		"decode 7fffffffffffffff" in {
			val it	= long"7fffffffffffffff"
			typed[Long](it)
			it mustEqual 9223372036854775807L
		}
		"decode 8000000000000000" in {
			val it	= long"8000000000000000"
			typed[Long](it)
			it mustEqual -9223372036854775808L
		}
		"decode ffffffffffffffff" in {
			val it	= long"ffffffffffffffff"
			typed[Long](it)
			it mustEqual -1L
		}
		
		"handle upper case letters" in {
			val it	= byte"Ea"
			typed[Byte](it)
			it mustEqual 234.toByte
		}
		
		/*
		"fail at compile time for invalid string" in {
		 	hex"xx" == 0
		}
		*/
	}
}
