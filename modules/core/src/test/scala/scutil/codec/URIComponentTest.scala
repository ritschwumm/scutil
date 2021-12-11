package scutil.codec

import minitest.*

object URIComponentTest extends SimpleTestSuite {
	test("URIComponent should roundtrip all usual chars (low)") {
		val str	= '\u0000' to '\ud7ff' map { _.toChar } mkString ""
		val	enc	= URIComponent.utf_8 encode str
		val	dec	= URIComponent.utf_8 decode enc
		assertEquals(dec, Right(str))
	}

	test("URIComponent should roundtrip all usual chars (high)") {
		val str	= '\ue000' to '\uffff' map { _.toChar } mkString ""
		val	enc	= URIComponent.utf_8 encode str
		val	dec	= URIComponent.utf_8 decode enc
		assertEquals(dec, Right(str))
	}

	test("URIComponent should roundtrip chars outside the 16 bit range") {
		val str	= Character toChars Integer.parseInt("00010400", 16) mkString ""
		val	enc	= URIComponent.utf_8 encode str
		val	dec	= URIComponent.utf_8 decode enc
		assertEquals(dec, Right(str))
	}

	//------------------------------------------------------------------------------

	test("URIComponent should encode plus as %2B") {
		assertEquals(
			URIComponent.utf_8 encode "+",
			"%2B"
		)
	}
	test("URIComponent should encode blank as %20") {
		assertEquals(
			URIComponent.utf_8 encode " ",
			"%20"
		)
	}
	test("URIComponent should decode plus as plus") {
		assertEquals(
			URIComponent.utf_8 decode "+",
			Right("+")
		)
	}

	//------------------------------------------------------------------------------

	private val interestingRaw	= "~!@#$%^&*(){}[]=:/,;?+'\"\\"
	private val interestingCode	= "~!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%2C%3B%3F%2B'%22%5C"

	test("URIComponent should encode interesting chars just like encodeURIComponent") {
		assertEquals(
			URIComponent.utf_8 encode interestingRaw,
			interestingCode
		)
	}

	test("URIComponent should decode interesting chars just like decodeURIComponent") {
		assertEquals(
			URIComponent.utf_8 decode interestingCode,
			Right(interestingRaw)
		)
	}

	//------------------------------------------------------------------------------

	test("URIComponent should encode german umlauts") {
		assertEquals(
			URIComponent.utf_8 encode "äöü",
			"%C3%A4%C3%B6%C3%BC"
		)
	}

	test("URIComponent should decode german umlauts") {
		assertEquals(
			URIComponent.utf_8 decode "%C3%A4%C3%B6%C3%BC",
			Right("äöü")
		)
	}

	//------------------------------------------------------------------------------

	test("URIComponent should fail with invalid % sequences (1)") {
		assertEquals(
			URIComponent.utf_8 decode "%",
			Left(URIComponentProblem.Invalid((1)))
		)
	}

	test("URIComponent should fail with invalid % sequences (2)") {
		assertEquals(
			URIComponent.utf_8 decode " %",
			Left(URIComponentProblem.Invalid((2)))
		)
	}

	test("URIComponent should fail with invalid % sequences (3)") {
		assertEquals(
			URIComponent.utf_8 decode "%x",
			Left(URIComponentProblem.Invalid((1)))
		)
	}

	test("URIComponent should fail with invalid % sequences (4)") {
		assertEquals(
			URIComponent.utf_8 decode "%1x",
			Left(URIComponentProblem.Invalid((2)))
			)
	}

	test("URIComponent should fail with invalid % sequences (5)") {
		assertEquals(
			URIComponent.utf_8 decode "%%",
			Left(URIComponentProblem.Invalid((1)))
		)
	}

	//------------------------------------------------------------------------------


	/*
	import javax.script.ScriptEngineManager

	test("URIComponent should encode everything just like encodeURIComponent") {
		val engine	= new ScriptEngineManager getEngineByName "JavaScript"
		val str	= 0 until 256 map { _.toChar } mkString ""
		val s1	= URIComponent.utf_8 encode (str, utf_8)
		engine put ("str", str)
		val s2	= engine eval """encodeURIComponent(str)"""
		assertEquals(s1, s2)
	}

	test("URIComponent should decode everything just like encodeURIComponent") {
		val engine	= new ScriptEngineManager getEngineByName "JavaScript"
		val str	= 0 until 256 map { _.toChar } mkString ""
		engine put ("str", str)
		val s1	= (engine eval """encodeURIComponent(str)""").asInstanceOf[String]
		val s2	= URIComponent.utf_8 decode (s1, utf_8)
		assertEquals(s2, str)
	}
	*/
}
