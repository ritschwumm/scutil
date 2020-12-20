package scutil.lang

import minitest._

final case class Named(a:Int, b:String, c:java.util.Date)

object FieldNamesTest extends SimpleTestSuite {
	test("FieldNames should work directly") {
		assertEquals(
			implicitly[FieldNames[Named]],
			FieldNames(Vector("a", "b", "c"))
		)
	}

	test("FieldNames should work indirectly") {
		val names	= getNames[Named]
		assertEquals(
			names,
			Seq("a", "b", "c")
		)
	}

	//------------------------------------------------------------------------------

	private def getNames[T:FieldNames]:Seq[String]	=
		implicitly[FieldNames[Named]].names
}
