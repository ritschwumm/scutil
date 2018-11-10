package scutil.lang

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import scutil.lang.implicits._

/** creates lens instances for a case classes' fields */
object LensGen {
	def apply[T]	= new LensGen[T]
}

final class LensGen[T] extends Dynamic {
	def selectDynamic(propName:String):AnyRef	= macro LensGenImpl.compile[T]
}

private final class LensGenImpl(val c:Context) {
	import c.universe._

	def compile[T:c.WeakTypeTag](propName:c.Tree):c.Tree	= {
		val out:Either[String,Tree]	=
				for {
					name			<-
							propName
							.matchOption	{ case Literal(Constant(name:String))	=> name }
							.toRight		(s"unexpected propName: ${propName}")
					fieldName		= TermName(name)
					containerType	= c.weakTypeOf[T]
					member			<-
							(containerType member fieldName)
							.optionBy		{ _ != NoSymbol }
							.toRight		(s"value ${name} is not a member of ${containerType}")
					valueType		<-
							(member typeSignatureIn containerType)
							.matchOption	{ case NullaryMethodType(tpe) => tpe }
							.toRight		(s"member ${name} of ${containerType} is not a field")
					containerName	= TermName("c$")
					valueName		= TermName("v$")
				}
				yield q"""
					_root_.scutil.lang.Lens[$containerType,$valueType](
						get	= ($containerName:$containerType) => $containerName.$fieldName,
						set	= ($valueName:$valueType) => ($containerName:$containerType) => $containerName.copy($fieldName=$valueName)
					)
				"""

		out cata (
			c abort (c.enclosingPosition, _),
			c untypecheck _
		)
	}
}
