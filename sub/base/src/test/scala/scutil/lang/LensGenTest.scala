package scutil.lang

import org.specs2.mutable._

final case class Foo(a:Int, b:String)
final case class Bar[T](t:T)

class LensGenTest extends Specification {
	"lenses" should {
		"work with simple case class getters" in {
			val container	= Foo(4711, "hallo")
			val lens		= LensGen[Foo].a
			lens get container mustEqual 4711
		}
		
		"work with simple case class setters" in {
			val container	= Foo(4711, "hallo")
			val lens		= LensGen[Foo].a
			lens put (container, 1337) mustEqual Foo(1337, "hallo")
		}
		
		"work with type parameterized case class getters" in {
			val container	= Bar("test")
			val lens		= LensGen[Bar[String]].t
			lens get container mustEqual "test"
		}
		
		"work with type parameterized case class setters" in {
			val container	= Bar("test")
			val lens		= LensGen[Bar[String]].t
			lens put (container, "haha") mustEqual Bar("haha")
		}
		
		"allow splitting lenser and lens creation" in {
			val container	= Foo(4711, "hallo")
			val lenses		= LensGen[Foo]
			val lens		= lenses.a
			lens put (container, 1337) mustEqual Foo(1337, "hallo")
		}
		
		"work as TLens.Gen" in {
			val container	= Foo(4711, "hallo")
			val lens		= TLens.Gen[Foo].a
			lens put (container, 1337) mustEqual Foo(1337, "hallo")
		}
	}
}
