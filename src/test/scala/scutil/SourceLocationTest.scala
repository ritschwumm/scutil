package scutil

import org.specs2.mutable._

class SourceLocationTest extends Specification {
	case class Check(x:Int)
	implicit val check:Check	= Check(1)
	
	"implicit SourceLocation" should {
		"be available with only an implicit parameter list" in {
			foo mustEqual SourceLocation("SourceLocationTest.scala", 11)
		}
		"be available with one normal and an implicit parameter list" in {
			bar(1) mustEqual SourceLocation("SourceLocationTest.scala", 14)
		}
		"be available with multiple normal and an implicit parameter list" in {
			quux(1)(1) mustEqual SourceLocation("SourceLocationTest.scala", 17)
		}
		
		"work with multiple implicit parameters" in {
			xxx mustEqual SourceLocation("SourceLocationTest.scala", 21)
		}
		"work with multiple implicit parameters" in {
			yyy mustEqual SourceLocation("SourceLocationTest.scala", 24)
		}
	}
	
	private def foo(implicit loc:SourceLocation):SourceLocation	= loc
	private def bar(x:Int)(implicit loc:SourceLocation):SourceLocation	= loc
	private def quux(x:Int)(y:Int)(implicit loc:SourceLocation):SourceLocation	= loc
	
	private def xxx(implicit loc:SourceLocation, x:Check):SourceLocation	= loc
	private def yyy(implicit x:Check, loc:SourceLocation):SourceLocation	= loc
}
