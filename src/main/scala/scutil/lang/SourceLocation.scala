package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.Context

object SourceLocation {
	implicit def sourceLocation:SourceLocation	= macro sourceLocationImpl
	 
	def sourceLocationImpl(c:Context):c.Expr[SourceLocation]	= {
		import c.universe._
		val pos	= c.macroApplication.pos
		// NOTE pos.column expands tab chars as 8 columns
		reify { 
			SourceLocation(
				// pos.source.file.file.getName
				// (c literal pos.source.file.path).splice,
				(c literal pos.source.file.name).splice,
				// (c literal (pos pointOrElse -1)).splice,
				(c literal pos.line).splice
			) 
		}
	}
}

final case class SourceLocation(
	name:String,
	line:Int
)
