package scutil.lang

import org.specs2.mutable._

import scutil.implicits._

final case class Named(a:Int, b:String, c:java.util.Date)

class FielderTest extends Specification {
	"fielding" should {
		"work directly" in {
			Fielder[Named] mustEqual Seq("a", "b", "c")
		}
		"work indirectly" in {
			val names	= getNames[Named]
			getNames[Named] mustEqual Seq("a", "b", "c")
		}
	}
	
	def getNames[T:Fielding]:Seq[String]	= Fielder[T]
}
