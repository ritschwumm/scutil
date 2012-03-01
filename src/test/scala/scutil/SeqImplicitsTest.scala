package scutil

import org.specs2.mutable._

import Implicits._

class SeqImplicitsTest extends Specification {
	"equivalentSpans" should {
		"be empty for empty input" in {
			(Seq.empty[String] equivalentSpans criterium) must haveTheSameElementsAs(Seq.empty)
		}
		"be simple for a 1-element input" in {
			(Seq("hallo") equivalentSpans criterium) must haveTheSameElementsAs(Seq(Seq("hallo")))
		}
		"group together 2 equivalent elements" in {
			(Seq("hallo", "hello") equivalentSpans criterium) must haveTheSameElementsAs(Seq(Seq("hallo", "hello")))
		}
		"group separate 2 non-equivalent elements" in {
			(Seq("hallo", "ballo") equivalentSpans criterium) must haveTheSameElementsAs(Seq(Seq("hallo"), Seq("ballo")))
		}
		"leave 1 non-equivalent element at the end" in {
			(Seq("hallo", "hello", "ballo") equivalentSpans criterium) must haveTheSameElementsAs(Seq(Seq("hallo", "hello"), Seq("ballo")))
		}
	}
	
	private def criterium(a:String,b:String):Boolean	=
			(a charAt 0) == (b charAt 0)
}
