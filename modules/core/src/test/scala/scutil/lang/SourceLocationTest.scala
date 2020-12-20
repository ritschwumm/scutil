package scutil.lang

import minitest._

@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
object SourceLocationTest extends SimpleTestSuite {
	final case class Check(x:Int)
	implicit val check:Check	= Check(1)

	//------------------------------------------------------------------------------

	test("implicit SourceLocation should be available with only an implicit parameter list") {
		assertEquals(
			foo,
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 14)
		)
	}

	test("implicit SourceLocation should be available with one normal and an implicit parameter list") {
		assertEquals(
			bar(1),
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 21)
		)
	}

	test("implicit SourceLocation should be available with multiple normal and an implicit parameter list") {
		assertEquals(
			quux(1)(1),
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 28)
		)
	}

	test("implicit SourceLocation should work with multiple implicit parameters") {
		assertEquals(
			xxx,
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 35)
		)
	}

	test("implicit SourceLocation should work with multiple implicit parameters") {
		assertEquals(
			yyy,
			SourceLocation(Some("modules/core/src/test/scala/scutil/lang/SourceLocationTest.scala"), "SourceLocationTest.scala", 42)
		)
	}

	//------------------------------------------------------------------------------

	private def foo(implicit loc:SourceLocation):SourceLocation	= loc
	private def bar(x:Int)(implicit loc:SourceLocation):SourceLocation			= loc
	private def quux(x:Int)(y:Int)(implicit loc:SourceLocation):SourceLocation	= loc

	private def xxx(implicit loc:SourceLocation, x:Check):SourceLocation	= { val _ = x; loc }
	private def yyy(implicit x:Check, loc:SourceLocation):SourceLocation	= { val _ = x; loc }
}
