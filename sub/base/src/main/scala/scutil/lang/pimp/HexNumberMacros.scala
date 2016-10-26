package scutil.lang.pimp

import scala.reflect.macros.blackbox.Context

import scutil.lang.Hex

private final class HexNumberMacros(val c:Context) {
	import c.universe._
	
	def byteImpl():c.Tree	= impl(1,	_.byteValue)
	def shortImpl():c.Tree	= impl(2,	_.shortValue)
	def intImpl():c.Tree	= impl(4,	_.intValue)
	def longImpl():c.Tree	= impl(8,	_.longValue)
	
	private def impl[T:Liftable](byteCount:Int, func:BigInt=>T):c.Tree	= {
		val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
		parse(str, byteCount, func) match {
			case Some(value)	=> q"$value"
			case None			=> c abort (c.enclosingPosition, s"invalid hex literal ${str}")
		}
	}
	
	private def parse[T](s:String, byteCount:Int, func:BigInt=>T):Option[T]	=
			(Hex bytes s)
			.filter (_.length == byteCount)
			.map	(BigInt(_))
			.map	(func)
}
