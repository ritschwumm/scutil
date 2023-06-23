package scutil.lang

import scala.annotation.nowarn

import minitest.*

object SourceLocationTest extends SimpleTestSuite {
	final case class Check(x:Int)
	given check:Check	= Check(1)

	//------------------------------------------------------------------------------

	test("implicit SourceLocation should be available with only an implicit parameter list") {
		assertEquals(
			foo,
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 15)
		)
	}

	test("implicit SourceLocation should be available with one normal and an implicit parameter list") {
		assertEquals(
			bar(1),
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 22)
		)
	}

	test("implicit SourceLocation should be available with multiple normal and an implicit parameter list") {
		assertEquals(
			quux(1)(1),
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 29)
		)
	}

	test("implicit SourceLocation should work with multiple implicit parameters") {
		assertEquals(
			xxx,
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 36)
		)
	}

	test("implicit SourceLocation should work with multiple implicit parameters") {
		assertEquals(
			yyy,
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 43)
		)
	}

	//------------------------------------------------------------------------------

	private def foo(using loc:SourceLocation):SourceLocation								= loc
	private def bar(@nowarn x:Int)(using loc:SourceLocation):SourceLocation					= loc
	private def quux(@nowarn x:Int)(@nowarn y:Int)(using loc:SourceLocation):SourceLocation	= loc

	private def xxx(using loc:SourceLocation, x:Check):SourceLocation	= { val _ = x; loc }
	private def yyy(using x:Check, loc:SourceLocation):SourceLocation	= { val _ = x; loc }
}
