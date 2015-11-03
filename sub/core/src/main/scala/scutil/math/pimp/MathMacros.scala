package scutil.math.pimp

import java.math.{ BigInteger => JBigInteger }

import scala.reflect.macros.blackbox.Context

import scutil.math.BigRational

private final class MathMacros(val c:Context) {
	import c.universe._
	
	private implicit val liftJBigInteger:Liftable[JBigInteger]	=
			Liftable[JBigInteger] { it =>
				val bar	= it.toByteArray
				q"new _root_.java.math.BigInteger($bar)"
			}

	private implicit val liftBigRational:Liftable[BigRational]	=
			Liftable[BigRational] { it =>
				val num	= it.numerator
				val den	= it.denominator
				q"_root_.scutil.math.BigRational($num, $den)"
			}

	def brImpl():c.Tree		= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		BigRational parse str match {
			case Some(value)	=> q"$value"
			case None			=> c abort (c.enclosingPosition, s"invalid BigRational literal ${str}")
		}
	}
}
