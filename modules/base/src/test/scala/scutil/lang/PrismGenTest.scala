package scutil.lang

import org.specs2.mutable._

sealed trait PrismSuper
	  case object	PrismObject			extends PrismSuper
final case class	PrismClass(a:Int)	extends PrismSuper

class PrismGenTest extends Specification {
	"PrismGen" should {
		"get a case object" in {
			val prism					= PrismGen[PrismSuper,PrismObject.type]
			val sub:PrismObject.type	= PrismObject
			val sup:PrismSuper			= sub
			prism get sup mustEqual Some(sub)
		}
		"put a case object" in {
			val prism					= PrismGen[PrismSuper,PrismObject.type]
			val sub:PrismObject.type	= PrismObject
			val sup:PrismSuper			= sub
			prism set sub mustEqual sup
		}
		
		"get a case class" in {
			val prism			= PrismGen[PrismSuper,PrismClass]
			val sub:PrismClass	= PrismClass(1)
			val sup:PrismSuper	= sub
			prism get sup mustEqual Some(sub)
		}
		"put a case class" in {
			val prism			= PrismGen[PrismSuper,PrismClass]
			val sub:PrismClass	= PrismClass(1)
			val sup:PrismSuper	= sub
			prism set sub mustEqual sup
		}
	}
}
