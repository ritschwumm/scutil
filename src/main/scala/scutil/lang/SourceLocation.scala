package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.Context

object SourceLocation {
	implicit def sourceLocation:SourceLocation	= macro sourceLocationImpl
	 
	def sourceLocationImpl(c:Context):c.Expr[SourceLocation]	= {
		import c.universe._
		val fun	= (c enclosingImplicits 0)._2 match {
			case fun:Select		=> fun	// method with only the implicit parameter list
			case Apply(fun, _)	=> fun	// method other parameter lists
		}
		val fileName	= fun.pos.source.file.name	// fun.pos.source.file.file.getName
		val line		= fun.pos.line				// fun.pos.point
		reify { 
			SourceLocation(
				(c literal fileName)	.splice,
				(c literal line)		.splice
			) 
		}
	}
}

case class SourceLocation(
	fileName:String, 
	line:Int
) {
	override def toString:String	= fileName + ":" + line
}
