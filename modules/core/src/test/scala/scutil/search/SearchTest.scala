package scutil.search

import org.specs2.mutable._

import scutil.lang._
import scutil.lang.implicits._

class SearchTest extends Specification {
	def single(s:String):Predicate[String]			= s into SearchParser.parse into SearchCompiler.single
	def multi(s:String):Predicate[Iterable[String]]	= s into SearchParser.parse into SearchCompiler.multi
	
	"TokenMatcher for a single string" should {
		"work 1"  in { single	("oo")			("foobar")	mustEqual true	}
		"work 2"  in { single	("|oo")			("foobar")	mustEqual false	}
		"work 3"  in { single	("oo|")			("foobar")	mustEqual false	}
		"work 4"  in { single	("|fo")			("foobar")	mustEqual true	}
		"work 5"  in { single	("|fo|")		("foobar")	mustEqual false	}
		"work 6"  in { single	("ar|")			("foobar")	mustEqual true	}
		"work 7"  in { single	("|ar|")		("foobar")	mustEqual false	}
		"work 8"  in { single	("foobar")		("foobar")	mustEqual true	}
		"work 9"  in { single	("|foobar")		("foobar")	mustEqual true	}
		"work 10" in { single	("foobar|")		("foobar")	mustEqual true	}
		"work 11" in { single	("|foobar|")	("foobar")	mustEqual true	}
		"work 12" in { single	("OO")			("foobar")	mustEqual false	}
		"work 13" in { single	("oo")			("FOOBAR")	mustEqual true	}
		"work 14" in { single	("OO")			("FOOBAR")	mustEqual true	}
		"work 15" in { single	("Oo")			("FOOBAR")	mustEqual false	}
		"work 16" in { single	("Oo")			("FOoBAR")	mustEqual true	}
	}
	"TokenMatcher for multiple strings" should {
		"work 1" in { multi	("a b")		(Seq("a", "b"))		mustEqual true	}
		"work 1" in { multi	("-a b")	(Seq("a", "b"))		mustEqual false	}
		"work 1" in { multi	("a -b")	(Seq("a", "b"))		mustEqual false	}
		"work 1" in { multi	("-a -b")	(Seq("a", "b"))		mustEqual false	}
		"work 1" in { multi	("a b")		(Seq("a", "c"))		mustEqual false	}
		"work 1" in { multi	("-a b")	(Seq("a", "c"))		mustEqual false	}
		"work 1" in { multi	("a -b")	(Seq("a", "c"))		mustEqual true	}
		"work 1" in { multi	("-a -b")	(Seq("a", "c"))		mustEqual false	}
	}
}
