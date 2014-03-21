package scutil

import java.lang.{ Long=>JLong }

import scala.language.experimental.macros
import scala.reflect.macros.Context
	
package object color {
	/** provides a string interpolator for web-style hex colors */
	implicit class HexContext(sc:StringContext) {
		def rgb():RGB	= macro HexContextMacros.rgbImpl
		def rgba():RGBA	= macro HexContextMacros.rgbaImpl
	}
	
	private object HexContextMacros {
		def rgbImpl(c:Context)():c.Expr[RGB]	= {
			import c.universe._
			// println(c.universe.showRaw(c.prefix.tree))
			val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
			RGB parseHex str match {
				case Some(RGB(r,g,b))	=> 
						reify {
							RGB(
								(c literal r).splice,
								(c literal g).splice,
								(c literal b).splice
							) 
						}
						// TODO do we need to call resetAllAttrs here?
				case None	=>
						(c abort (c.enclosingPosition, s"invalid rgb literal ${str}"))
			}
		}
		
		def rgbaImpl(c:Context)():c.Expr[RGBA]	= {
			import c.universe._
			// println(c.universe.showRaw(c.prefix.tree))
			val Apply(_, List(Apply(_, List(Literal(Constant(str:String))))))	= c.prefix.tree
			RGBA parseHex str match {
				case Some(RGBA(RGB(r,g,b),Alpha(a)))	=> 
						reify {
							RGBA(
								RGB(
									(c literal r).splice,
									(c literal g).splice,
									(c literal b).splice
								),
								Alpha(
									(c literal a).splice
								)
							) 
						}
						// TODO do we need to call resetAllAttrs here?
				case None	=>
						(c abort (c.enclosingPosition, s"invalid rgba literal ${str}"))
			}
		}
	}
}
