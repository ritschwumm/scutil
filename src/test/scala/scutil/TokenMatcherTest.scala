package scutil

import org.specs2.mutable._

import search._

class SearchTest extends Specification {
	"TokenMatcher for a single string" should {
		"work 1" in { SearchUtil.compile	("oo")			("foobar")	mustEqual true	} 
		"work 2" in { SearchUtil.compile	("|oo")			("foobar")	mustEqual false	}
		"work 3" in { SearchUtil.compile	("oo|")			("foobar")	mustEqual false	}
		"work 4" in { SearchUtil.compile	("|fo")			("foobar")	mustEqual true	}
		"work 5" in { SearchUtil.compile	("|fo|")		("foobar")	mustEqual false	}
		"work 6" in { SearchUtil.compile	("ar|")			("foobar")	mustEqual true	}
		"work 7" in { SearchUtil.compile	("|ar|")		("foobar")	mustEqual false	}
		"work 8" in { SearchUtil.compile	("foobar")		("foobar")	mustEqual true	}
		"work 9" in { SearchUtil.compile	("|foobar")		("foobar")	mustEqual true	}
		"work 10" in { SearchUtil.compile	("foobar|")		("foobar")	mustEqual true	}
		"work 11" in { SearchUtil.compile	("|foobar|")	("foobar")	mustEqual true	}
		"work 12" in { SearchUtil.compile	("OO")			("foobar")	mustEqual false	}
		"work 13" in { SearchUtil.compile	("oo")			("FOOBAR")	mustEqual true	}
		"work 14" in { SearchUtil.compile	("OO")			("FOOBAR")	mustEqual true	}
		"work 15" in { SearchUtil.compile	("Oo")			("FOOBAR")	mustEqual false	}
		"work 16" in { SearchUtil.compile	("Oo")			("FOoBAR")	mustEqual true	}
	}
	"TokenMatcher for multiple strings" should {
		"work 1" in { SearchUtil.compileSeq	("a b")		(Seq("a", "b"))		mustEqual true	}
		"work 1" in { SearchUtil.compileSeq	("-a b")	(Seq("a", "b"))		mustEqual false	}
		"work 1" in { SearchUtil.compileSeq	("a -b")	(Seq("a", "b"))		mustEqual false	}
		"work 1" in { SearchUtil.compileSeq	("-a -b")	(Seq("a", "b"))		mustEqual false	}
		"work 1" in { SearchUtil.compileSeq	("a b")		(Seq("a", "c"))		mustEqual false	}
		"work 1" in { SearchUtil.compileSeq	("-a b")	(Seq("a", "c"))		mustEqual false	}
		"work 1" in { SearchUtil.compileSeq	("a -b")	(Seq("a", "c"))		mustEqual true	}
		"work 1" in { SearchUtil.compileSeq	("-a -b")	(Seq("a", "c"))		mustEqual false	}
	}
}
