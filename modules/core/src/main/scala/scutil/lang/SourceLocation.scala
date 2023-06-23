package scutil.lang

import java.nio.file.Path
import java.nio.file.Paths

import scala.quoted.*

object SourceLocation {
	inline given SourceLocation	=  ${sourceLocationImpl}

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

	private def relative(path:Path):String	=
		currentDir.toAbsolutePath.relativize(path).toString

	private def currentDir:Path	=
		Paths.get("")
}

final case class SourceLocation(
	path:Option[String],
	name:String,
	line:Int,
) {
	override def toString:String	= name + ":" + line.toString + path.map(path => s" (${path})").getOrElse("")
}
