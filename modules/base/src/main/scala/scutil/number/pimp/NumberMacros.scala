package scutil.number.pimp

import java.math.{ BigInteger => JBigInteger }

import scala.reflect.macros.blackbox.Context

import scutil.number.BigRational

private final class NumberMacros(val c:Context) {
	import c.universe._

	// NOTE linter error if private
	protected implicit val liftJBigInteger:Liftable[JBigInteger]	=
			Liftable[JBigInteger] { it =>
				val bar	= it.toByteArray
				q"new _root_.java.math.BigInteger($bar)"
			}

	// NOTE linter error if private
	protected implicit val liftBigRational:Liftable[BigRational]	=
			Liftable[BigRational] { it =>
				val num:JBigInteger	= it.numerator
				val den:JBigInteger	= it.denominator
				q"""_root_.scutil.number.BigRational($num, $den).getOrElse(sys error "unexpected zero denominator")"""
			}

	def brImpl():c.Tree		= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		BigRational parse str match {
			case Some(value)	=> q"$value"
			case None			=> c abort (c.enclosingPosition, s"invalid BigRational literal ${str}")
		}
	}
}
