package scutil.lang

import scala.language.dynamics
import scala.quoted.*

/** creates lens instances for a case classes' fields */
object LensGen {
	def apply[T]	= new LensGen[T]
}

// NOTE dotty Selectable is not good enough for this
final class LensGen[T] extends Dynamic {
	inline transparent def selectDynamic(inline name:String):Any	= ${ LensGenImpl.selectImpl[T]('name) }
}

object LensGenImpl {
	// NOTE needs a transparent call to make the return type flexible
	@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
	def selectImpl[T:Type](name:Expr[String])(using quotes:Quotes):Expr[Any] = {
		import quotes.reflect.*

		val containerTypeRepr	= TypeRepr.of[T].dealias

		val containerTypeSymbol:Symbol	=
			containerTypeRepr.typeSymbol

		val containerGenericTypeArguments	=
			containerTypeRepr match {
				case AppliedType(_, argTypeReprs)	=> argTypeReprs
				case _								=> Nil
			}

		val nameValue:String	= name.valueOrAbort

		val caseFieldSymbol:Symbol	=
			containerTypeSymbol.caseFields.find(it => it.name == nameValue).getOrElse {
				report.errorAndAbort(
					s"field ${containerTypeSymbol.fullName}#${name} does not exist",
					// put the error on the name
					name.asTerm.pos
				)
			}


		// NOTE this leads to a different result type - why?
		//val fieldType =
		//	containerTypeRepr.select(caseField)
		val fieldType:TypeRepr =
			containerTypeRepr.memberType(caseFieldSymbol)

		// TODO dotty check alternatives
		// def getterTerm(container:Term):Term	= Typed(Select.unique(container, nameValue), fieldType)
		// def getterTerm(container:Term):Term	= Select.unique(container, nameValue)
		// NOTE TypeTree.of[T] would work, too
		//def getterTerm(container:Term):Term	= Typed(Select.unique(container, nameValue), Inferred(fieldType))
		// NOTE compiles, but doesn't help

		def getterTerm(container:Term):Term	=
			container.select(caseFieldSymbol)

		def setterTerm(container:Term, value:Term):Term	=
		 	Select.overloaded(container, "copy", containerGenericTypeArguments, List(NamedArg(caseFieldSymbol.name, value)))


		// NOTE we can match on Type value and recover the type's type like this!
		// @see https://softwaremill.com/scala-3-macros-tips-and-tricks/
		(containerTypeRepr.asType, fieldType.asType) match { case ('[c], '[f]) =>
			'{
				Lens(
					(s:c) => ${
						getterTerm('{s}.asTerm).asExprOf[f]
					},
					(t:f) => (s:c) => ${
						setterTerm('{s}.asTerm, '{t}.asTerm).asExprOf[c]
					}
				)
			}
		}
	}
}
