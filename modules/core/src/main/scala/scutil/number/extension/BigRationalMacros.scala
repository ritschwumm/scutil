package scutil.number.extension

import java.math.{ BigInteger => JBigInteger }

import scala.reflect.macros.blackbox.Context

import scutil.number.BigRational

private final class BigRationalMacros(val c:Context) {
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

	def brImpl():c.Tree		=
		c.prefix.tree match {
			case Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	=>
				BigRational parse str match {
					case Some(value)	=> q"$value"
					case None			=> c.abort(c.enclosingPosition, s"invalid BigRational literal ${str}")
				}
			case x =>
				c.abort(c.enclosingPosition, s"invalid BigRational literal ${x.toString}")
		}
}
