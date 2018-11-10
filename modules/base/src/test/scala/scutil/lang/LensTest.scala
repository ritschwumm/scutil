package scutil.lang

import org.specs2.mutable._

object LensTest extends Specification {
	final case class A(x:String, b:B)
	final case class B(y:String, c:Int)

	"lenses" should {
		"just work" in {
			val a	= A("a", B("b", 1))

			val Ab	= Lens((a:A) => a.b, (b:B)		=> (a:A)	=> a copy (b=b))
			val Bc	= Lens((b:B) => b.c, (c:Int)	=> (b:B)	=> b copy (c=c))

			val ABc	= Ab andThen Bc

			val result	= ABc set 2 apply a

			result mustEqual A("a",B("b",2))
		}
	}
}
