package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object SourceLocation {
	implicit def sourceLocation:SourceLocation	= macro sourceLocationImpl
	 
	def sourceLocationImpl(c:Context):c.Tree	= {
		import c.universe._
		val pos	= c.macroApplication.pos
		// NOTE pos.column expands tab chars as 8 columns
		// pos.source.file.path
		// pos pointOrElse -1
		q"_root_.scutil.lang.SourceLocation(${pos.source.file.name}, ${pos.line})"
	}
}

final case class SourceLocation(
	name:String,
	line:Int
) {
	override def toString:String	= name + ":" + line
}
