package scutil.lang

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scutil.lang.implicits._

object FieldNames {
	implicit def of[T]:FieldNames[T]	= macro ofImpl[T]
	
	def ofImpl[T:c.WeakTypeTag](c:Context):c.Tree	= {
		import c.universe._
		
		val selfType:Type	= weakTypeOf[T]
		
		val primaryCtors	=
				selfType.decls
				.filter	{ _.isMethod }
				.map	{ _.asMethod }
				.filter	{ _.isPrimaryConstructor }
			
		val names:Either[String,Tree]	=
				for {
					primaryCtor		<- singleOption(primaryCtors)			toRight s"primary constructor not found in ${selfType}"
					paramNames		<- singleOption(primaryCtor.paramLists)	toRight s"primary constructor has multiple parameter lists in ${selfType}"
					decodedNames	= paramNames map { _.name.decodedName.toString }
				}
				yield {
					q"_root_.scutil.lang.FieldNames[$selfType](_root_.scala.collection.immutable.Vector(..$decodedNames))"
				}
				
		names cata (
			c abort (c.enclosingPosition, _),
			c untypecheck _
		)
	}
	
	private def singleOption[T](it:Iterable[T]):Option[T]	=
			if (it.size == 1)	Some(it.head)
			else				None
}

final case class FieldNames[T](names:Vector[String])
