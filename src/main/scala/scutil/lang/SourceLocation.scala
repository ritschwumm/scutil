package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.Context

object SourceLocation {
	implicit def sourceLocation:SourceLocation	= macro sourceLocationImpl
	 
	def sourceLocationImpl(c:Context):c.Expr[SourceLocation]	= {
		import c.universe._
		
		val pos		= c.macroApplication.pos
		val name	= pos.source.file.name	// pos.source.file.file.getName
		val line	= pos.line				// pos.point
		
		reify { 
			SourceLocation(
				(c literal name).splice,
				(c literal line).splice
			) 
		}
	}
}

final case class SourceLocation(
	name:String, 
	line:Int
) {
	override def toString:String	= name + ":" + line
}
