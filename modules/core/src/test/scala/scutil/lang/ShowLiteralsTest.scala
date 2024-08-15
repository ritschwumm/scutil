package scutil.lang

import minitest.*

import scutil.lang.implicits.*

object ShowLiterals extends SimpleTestSuite {
	test("show interpolator should do an empty string") {
		assertEquals(
			show"""""",
			""
		)
	}

	test("show interpolator should do a single string") {
		assertEquals(
			show"""a""",
			"a"
		)
	}

	test("show interpolator should do a single value") {
		val a = "1"
		assertEquals(
			show"""$a""",
			"1"
		)
	}

	test("show interpolator should do a string and a value") {
		val a = "1"
		assertEquals(
			show"""${a}test""",
			"1test"
		)
	}

	test("show interpolator should do a value and a string") {
		val a = "1"
		assertEquals(
			show"""test${a}""",
			"test1"
		)
	}

	test("show interpolator should work with multiple values and types") {
		val a = "1"
		val b = 2
		val c = true
		assertEquals(
			show"""aaa${a}bbb${b}ccc${c}""",
			"aaa1bbb2ccctrue"
		)
	}

	//------------------------------------------------------------------------------

	import scutil.lang.tc.*
	given OptionShow[T:Show]:Show[Option[T]]	=
		Show instance {
			case Some(x)	=> "some: " + Show.doit(x)
			case None		=> "none"
		}

	test("show interpolator should work with custom instances") {
		val o:Option[Int]	= Some(1)
		assertEquals(
			show"""$o""",
			"some: 1"
		)
	}

	//------------------------------------------------------------------------------

	/*
	test("show interpolator should work with inheritance") {
		val o:Some[Int]	= Some(1)
		assertEquals(
			show"""$o""",
			"some: 1"
		)
	}
	*/

	//------------------------------------------------------------------------------

	test("show interpolator should allow escapes") {
		assertEquals(
			show"\t",
			"\t"
		)
	}

	test("show interpolator should allow escapes") {
		assertEquals(
			show"\u0000",
			s"\u0000"
		)
	}

	/*
	// fails at compile time, as it should
	test("show interpolator should disallow unknown escapes") {
		assertEquals(
			show"""\x""",
			s"\t"
		)
	}
	*/

	test("show interpolator should allow double quote escapes") {
		assertEquals(
			show"""\"""",
			s"""\""""
		)
	}

	test("show interpolator should allow double quote escapes") {
		assertEquals(
			show"""\"""",
			"\""
		)
	}
}
