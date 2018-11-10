package scutil.lang

import org.specs2.mutable._

final case class Named(a:Int, b:String, c:java.util.Date)

class FieldNamesTest extends Specification {
	"FieldNames" should {
		"work directly" in {
			implicitly[FieldNames[Named]] mustEqual FieldNames(Vector("a", "b", "c"))
		}
		"work indirectly" in {
			val names	= getNames[Named]
			names mustEqual Seq("a", "b", "c")
		}
	}

	def getNames[T:FieldNames]:Seq[String]	=implicitly[FieldNames[Named]].names
}
