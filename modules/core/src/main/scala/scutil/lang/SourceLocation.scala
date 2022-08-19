package scutil.lang

import java.io.File
import java.nio.file.Path

import scala.quoted.*

object SourceLocation {
	implicit inline def sourceLocation:SourceLocation	=  ${sourceLocationImpl}

	private def sourceLocationImpl(using quotes:Quotes):Expr[SourceLocation]	= {
		import quotes.reflect.*

		val pos = Position.ofMacroExpansion

		// @see https://scala-lang.org/api/3.x/scala/quoted/Quotes$reflectModule$PositionMethods.html
		// path, start, end, startLine, endLine, startColumn, endColumn, sourceCode
		val path	= Expr(pos.sourceFile.getJPath.map(relative))
		val name	= Expr(pos.sourceFile.name)
		val line	= Expr(pos.startLine + 1)

		'{ SourceLocation($path, $name, $line) }
	}

	// TODO dotty we should probably use Path.relativize here
	private def relative(path:Path):String	=
		new File(".").toURI.relativize(path.toUri).getPath
}

final case class SourceLocation(
	path:Option[String],
	name:String,
	line:Int,
) {
	override def toString:String	= name + ":" + line.toString + path.map(path => s" (${path})").getOrElse("")
}
