package scutil.lang

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.InvalidPathException

import scala.quoted.*

object SourceLocation {
	inline given SourceLocation	=  ${sourceLocationImpl}

	private def sourceLocationImpl(using quotes:Quotes):Expr[SourceLocation]	= {
		import quotes.reflect.*

		val pos = Position.ofMacroExpansion

		// @see https://scala-lang.org/api/3.x/scala/quoted/Quotes$reflectModule$PositionMethods.html
		// path, start, end, startLine, endLine, startColumn, endColumn, sourceCode

		// NOTE replaces .getJPath which is deprecated in scala 3.9.0 because it cannot deal with virtual files
		val jpath	= parsePath(pos.sourceFile.path).map(relativePath).map(_.toString)

		val path	= Expr(jpath)
		val name	= Expr(pos.sourceFile.name)
		val line	= Expr(pos.startLine + 1)

		'{ SourceLocation($path, $name, $line) }
	}

	//-----------------------------------------------------------------------------

	private def parsePath(path:String):Option[Path]	=
		try { Some(Path.of(path)) }
		catch { case _:InvalidPathException => None }

	private def relativePath(path:Path):Path	=
		currentDir.toAbsolutePath.relativize(path)

	private def currentDir:Path	=
		Paths.get("")
}

final case class SourceLocation(
	// NOTE this is empty for virtual files
	path:Option[String],
	name:String,
	line:Int,
) {
	override def toString:String	= name + ":" + line.toString + path.map(path => s" (${path})").getOrElse("")
}
