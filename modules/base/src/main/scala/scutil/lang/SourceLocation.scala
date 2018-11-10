package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import java.io.File

object SourceLocation {
	implicit def sourceLocation:SourceLocation	= macro sourceLocationImpl

	def sourceLocationImpl(c:Context):c.Tree	= {
		import c.universe._
		val pos	= c.macroApplication.pos
		// NOTE pos.column expands tab chars as 8 columns
		// pos.source.file.path
		// pos pointOrElse -1
		val path	=
				Option(pos.source.file.file) map { file =>
					(new File(".").toURI relativize file.toURI).getPath
				}
		q"_root_.scutil.lang.SourceLocation($path, ${pos.source.file.name}, ${pos.line})"
	}
}

final case class SourceLocation(
	path:Option[String],
	name:String,
	line:Int
) {
	override def toString:String	= name + ":" + line.toString
}
