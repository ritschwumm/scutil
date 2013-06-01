package scutil.lens

import org.specs2.mutable._

import scutil.lang._

object LensTest extends Specification {
	case class A(x:String, b:B)
	case class B(y:String, c:Int)
	
	"lenses" should {
		val a	= A("a", B("b", 1))
		
		val Ab	= TLens	create ((a:A) => a.b, (a:A,b:B)		=> a copy (b=b))
		val Bc	= TLens	create ((b:B) => b.c, (b:B,c:Int)	=> b copy (c=c))
		
		val ABc	= Ab andThen Bc
		
		val result	= ABc put (a, 2)
		
		result mustEqual A("a",B("b",2))
	}
}
