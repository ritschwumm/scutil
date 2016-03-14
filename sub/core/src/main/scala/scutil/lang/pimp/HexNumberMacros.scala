package scutil.lang.pimp

import java.math.{ BigInteger => JBigInt }

import scala.reflect.macros.blackbox.Context

private final class HexNumberMacros(val c:Context) {
	import c.universe._
	
	def byteImpl():c.Tree	= impl(2,	_.byteValue)
	def shortImpl():c.Tree	= impl(4,	_.shortValue)
	def intImpl():c.Tree	= impl(8,	_.intValue)
	def longImpl():c.Tree	= impl(16,	_.longValue)
	
	private def impl[T:Liftable](len:Int, func:JBigInt=>T):c.Tree	= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		parse(str, len, func) match {
			case Some(value)	=> q"$value"
			case None			=> c abort (c.enclosingPosition, s"invalid hex literal ${str}")
		}
	}
	
	private val hexBase		= 16
	private val hexDigits	= "[0-9a-fA-F]*"
	
	private def parse[T](s:String, len:Int, func:JBigInt=>T):Option[T]	=
			if (s.length != len)		None	else
			if (!(s matches hexDigits))	None	else
			try { Some(func(new JBigInt(s, hexBase))) }
			catch { case e:Exception => None }
}
