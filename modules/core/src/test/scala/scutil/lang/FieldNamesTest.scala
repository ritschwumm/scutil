package scutil.lang

import minitest.*

object FieldNamesTest extends SimpleTestSuite {
	final case class Named1(a:Int)
	final case class Named2(a:Int, b:String)
	final case class Named3(a:Int, b:String, c:java.util.Date)

	//-----------------------------------------------------------------------------

	test("FieldNames should work with 1 field") {
		assertEquals(
			summon[FieldNames[Named1]],
			FieldNames(Vector("a"))
		)
	}

	test("FieldNames should work with 2 fields") {
		assertEquals(
			summon[FieldNames[Named2]],
			FieldNames(Vector("a", "b"))
		)
	}


	test("FieldNames should work with 3 fields") {
		assertEquals(
			summon[FieldNames[Named3]],
			FieldNames(Vector("a", "b", "c"))
		)
	}

	//------------------------------------------------------------------------------

	test("FieldNames should work indirectly") {
		val names	= getNames[Named3]
		assertEquals(
			names,
			Seq("a", "b", "c")
		)
	}

	private def getNames[T:FieldNames]:Seq[String]	=
		summon[FieldNames[T]].names
}
