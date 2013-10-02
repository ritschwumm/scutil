package scutil

import java.lang.{ Long=>JLong }
import java.awt.Color

import scala.language.experimental.macros
import scala.reflect.macros.Context
	
package object color {
	/** provides a string interpolator for web-style hex colors */
	implicit class HexContext(sc:StringContext) {
		def rgb():RGB	= macro HexMacros.apply
	}
	
	private object HexMacros {
		def apply(c:Context)():c.Expr[RGB]	= {
			import c.universe._
			// println(c.universe.showRaw(c.prefix.tree))
			val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
			if (RGB hexString str) {
				val lit	= c literal str
				reify {
					RGB decodeHex lit.splice
				}
				// TODO do we need to call resetAllAttrs here?
			}
			else {
				(c abort (c.enclosingPosition, s"invalid rgb literal ${str}"))
			}
		}
	}
}
