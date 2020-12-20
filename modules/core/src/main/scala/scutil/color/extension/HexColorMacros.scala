package scutil.color.extension

import scala.reflect.macros.blackbox.Context

import scutil.color._

private final class HexColorMacros(val c:Context) {
	import c.universe._

	def rgbImpl():c.Tree	=
		c.prefix.tree match {
			case Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	=>
				RGB parseHex str match {
					case Some(RGB(r, g, b))	=>
						q"_root_.scutil.color.RGB($r, $g, $b)"
					case None	=>
						c.abort(c.enclosingPosition, s"invalid rgb literal ${str}")
				}
			case x =>
				c.abort(c.enclosingPosition, s"invalid rgb literal ${x.toString}")
		}

	def rgbaImpl():c.Tree	=
		c.prefix.tree match {
			case Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	=>
				RGBA parseHex str match {
					case Some(RGBA(RGB(r, g, b),Alpha(a)))	=>
						q"_root_.scutil.color.RGBA(_root_.scutil.color.RGB($r, $g, $b), _root_.scutil.color.Alpha($a))"
					case None	=>
						c.abort(c.enclosingPosition, s"invalid rgba literal ${str}")
				}
			case x =>
				c.abort(c.enclosingPosition, s"invalid rgb literal ${x.toString}")
		}
}
