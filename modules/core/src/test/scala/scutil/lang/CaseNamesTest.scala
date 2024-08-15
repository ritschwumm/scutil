package scutil.lang

import minitest.*

object CaseNamesTest extends SimpleTestSuite {
	enum CaseNamed {
		case Foo, Bar, Quux
	}

	//-----------------------------------------------------------------------------

	test("CaseNames should work") {
		assertEquals(
			summon[CaseNames[CaseNamed]],
			CaseNames(Vector("Foo", "Bar", "Quux"))
		)
	}

	//------------------------------------------------------------------------------

	test("CaseNames should work indirectly") {
		val names	= getNames[CaseNamed]
		assertEquals(
			names,
			Seq("Foo", "Bar", "Quux")
		)
	}

	private def getNames[T:CaseNames]:Seq[String]	=
		summon[CaseNames[T]].names
}
