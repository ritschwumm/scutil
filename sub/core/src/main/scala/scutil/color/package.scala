package scutil

import java.lang.{ Long=>JLong }

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
	
package object color {
	/** provides a string interpolator for web-style hex colors */
	implicit class HexContext(sc:StringContext) {
		def rgb():RGB	= macro HexContextMacros.rgbImpl
		def rgba():RGBA	= macro HexContextMacros.rgbaImpl
	}
	
	private final class HexContextMacros(val c:Context) {
		def rgbImpl():c.Tree	= {
			import c.universe._
			// println(c.universe.showRaw(c.prefix.tree))
			val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
			RGB parseHex str match {
				case Some(RGB(r,g,b))	=>
						// TODO do we need to call c.untypecheck here?
						q"scutil.color.RGB($r, $g, $b)"
				case None	=>
						c abort (c.enclosingPosition, s"invalid rgb literal ${str}")
			}
		}
		
		def rgbaImpl():c.Tree	= {
			import c.universe._
			// println(c.universe.showRaw(c.prefix.tree))
			val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
			RGBA parseHex str match {
				case Some(RGBA(RGB(r,g,b),Alpha(a)))	=>
						// TODO do we need to call c.untypecheck here?
						q"scutil.color.RGBA(scutil.color.RGB($r, $g, $b), scutil.color.Alpha($a))"
				case None	=>
						c abort (c.enclosingPosition, s"invalid rgba literal ${str}")
			}
		}
	}
}
