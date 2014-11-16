package scutil.lang

import org.specs2.mutable._

final case class Unary(a:Int)
final case class Binary(a:Int, b:Short)
final case class Container(x:Unary)
final case class UnaryOption(a:Option[Int])

trait Fooz[T]
final case class Parametrized1[A](x:Fooz[A])
final case class Parametrized2[A,B](x:Fooz[A], y:Fooz[B])

object Wrapper {
	case class Inner(t:Int)
	
	val bij	= Bijector[Inner]
}

//------------------------------------------------------------------------------

class BijectorTest extends Specification {
	"bijections" should {
		"work with unary case classes" in {
			val value		= Unary(1)
			val	bijection	= Bijector[Unary]
			bijection write value mustEqual 1
		}
		"work with binary case classes" in {
			val value		= Binary(1,2)
			val	bijection	= Bijector[Binary]
			bijection write value mustEqual (1,2)
		}
		"work with nested case classes" in {
			val value		= Container(Unary(1))
			val	bijection	= Bijector[Container]
			bijection write value mustEqual Unary(1)
		}
		"work with unary case classes where the argument is a Product" in {
			val value		= UnaryOption(Some(1))
			val	bijection	= Bijector[UnaryOption]
			bijection write value mustEqual Some(1)
		}
		"work with case classes with one type parameter" in {
			def	XXX[A] = Bijector[Parametrized1[A]]
			1 mustEqual 1
		}
		"work with case classes with multiple type parameters" in {
			def	XXX[A,B] = Bijector[Parametrized2[A,B]]
			1 mustEqual 1
		}
	}
}
