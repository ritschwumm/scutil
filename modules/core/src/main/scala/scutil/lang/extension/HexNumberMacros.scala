package scutil.lang.extension

import scala.reflect.macros.blackbox.Context

import scutil.codec.Hex

private final class HexNumberMacros(val c:Context) {
	import c.universe._

	def byteImpl():c.Tree	= impl(1,	_.byteValue)
	def shortImpl():c.Tree	= impl(2,	_.shortValue)
	def intImpl():c.Tree	= impl(4,	_.intValue)
	def longImpl():c.Tree	= impl(8,	_.longValue)

	private def impl[T:Liftable](byteCount:Int, func:BigInt=>T):c.Tree	=
		c.prefix.tree match {
			case Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	=>
				parse(str, byteCount, func) match {
					case Some(value)	=> q"$value"
					case None			=> c.abort(c.enclosingPosition, s"invalid hex number literal ${str}")
				}
			case x =>
				c.abort(c.enclosingPosition, s"invalid hex number literal ${x.toString}")
		}

	private def parse[T](s:String, byteCount:Int, func:BigInt=>T):Option[T]	=
		(Hex decodeByteString s)
		.filter (_.size == byteCount)
		.map	(it => BigInt(it.unsafeValue))
		.map	(func)
}
