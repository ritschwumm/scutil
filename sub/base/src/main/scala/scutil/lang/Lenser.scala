package scutil.lang

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import scutil.lang.implicits._

/** creates lens instances for a case classes' fields */
object Lenser {
	def apply[T]	= new Lenser[T]
}

final class Lenser[T] extends Dynamic {
	def selectDynamic(propName:String):AnyRef	= macro LenserImpl.compile[T]
}

private final class LenserImpl(val c:Context) {
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
							.guardBy		{ _ != NoSymbol }
							.toRight		(s"value ${name} is not a member of ${containerType}")
					valueType		<-
							(member typeSignatureIn containerType)
							.matchOption	{ case NullaryMethodType(tpe) => tpe }
							.toRight		(s"member ${name} of ${containerType} is not a field")
					containerName	= TermName("c$")
					valueName		= TermName("v$")
				}
				yield q"""
					_root_.scutil.lang.TLens.create[$containerType,$valueType](
						($containerName:$containerType) => $containerName.$fieldName,
						($containerName:$containerType, $valueName:$valueType) => $containerName.copy($fieldName=$valueName)
					)
				"""
				
		out cata (
			c abort (c.enclosingPosition, _),
			c untypecheck _
		)
	}
}
