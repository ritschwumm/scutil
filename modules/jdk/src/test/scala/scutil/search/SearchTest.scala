package scutil.search

import minitest.*

import scutil.lang.*
import scutil.lang.implicits.*

object SearchTest extends SimpleTestSuite {
	def single(s:String):Predicate[String]			= s |> SearchParser.parse |> SearchCompiler.single
	def multi(s:String):Predicate[Iterable[String]]	= s |> SearchParser.parse |> SearchCompiler.multi

	test("TokenMatcher for a single string should work 1")		{ assertEquals(single	("oo")			("foobar"),			true)	}
	test("TokenMatcher for a single string should work 2")		{ assertEquals(single	("|oo")			("foobar"),			false)	}
	test("TokenMatcher for a single string should work 3")		{ assertEquals(single	("oo|")			("foobar"),			false)	}
	test("TokenMatcher for a single string should work 4")		{ assertEquals(single	("|fo")			("foobar"),			true)	}
	test("TokenMatcher for a single string should work 5")		{ assertEquals(single	("|fo|")		("foobar"),			false)	}
	test("TokenMatcher for a single string should work 6")		{ assertEquals(single	("ar|")			("foobar"),			true)	}
	test("TokenMatcher for a single string should work 7")		{ assertEquals(single	("|ar|")		("foobar"),			false)	}
	test("TokenMatcher for a single string should work 8")		{ assertEquals(single	("foobar")		("foobar"),			true)	}
	test("TokenMatcher for a single string should work 9")		{ assertEquals(single	("|foobar")		("foobar"),			true)	}
	test("TokenMatcher for a single string should work 10")		{ assertEquals(single	("foobar|")		("foobar"),			true)	}
	test("TokenMatcher for a single string should work 11")		{ assertEquals(single	("|foobar|")	("foobar"),			true)	}
	test("TokenMatcher for a single string should work 12")		{ assertEquals(single	("OO")			("foobar"),			false)	}
	test("TokenMatcher for a single string should work 13")		{ assertEquals(single	("oo")			("FOOBAR"),			true)	}
	test("TokenMatcher for a single string should work 14")		{ assertEquals(single	("OO")			("FOOBAR"),			true)	}
	test("TokenMatcher for a single string should work 15")		{ assertEquals(single	("Oo")			("FOOBAR"),			false)	}
	test("TokenMatcher for a single string should work 16")		{ assertEquals(single	("Oo")			("FOoBAR"),			true)	}

	test("TokenMatcher for a multiple string should work 1")	{ assertEquals(multi	("a b")			(Seq("a", "b")),	true)	}
	test("TokenMatcher for a multiple string should work 2")	{ assertEquals(multi	("-a b")		(Seq("a", "b")),	false)	}
	test("TokenMatcher for a multiple string should work 3")	{ assertEquals(multi	("a -b")		(Seq("a", "b")),	false)	}
	test("TokenMatcher for a multiple string should work 4")	{ assertEquals(multi	("-a -b")		(Seq("a", "b")),	false)	}
	test("TokenMatcher for a multiple string should work 5")	{ assertEquals(multi	("a b")			(Seq("a", "c")),	false)	}
	test("TokenMatcher for a multiple string should work 6")	{ assertEquals(multi	("-a b")		(Seq("a", "c")),	false)	}
	test("TokenMatcher for a multiple string should work 7")	{ assertEquals(multi	("a -b")		(Seq("a", "c")),	true)	}
	test("TokenMatcher for a multiple string should work 8")	{ assertEquals(multi	("-a -b")		(Seq("a", "c")),	false)	}
}
