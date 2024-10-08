package scutil.lang

import scala.reflect.Enum
import scala.quoted.*

/*
for reflected types
@see https://dotty.epfl.ch/api/scala/quoted/Quotes$reflectModule.html

for macros in general
@see https://softwaremill.com/scala-3-macros-tips-and-tricks/
@see https://eed3si9n.com/intro-to-scala-3-macros/

about enum macros in general
@see https://stackoverflow.com/search?q=%5Bscala-macros%5D+%5Bscala-3%5D+enum

for this specific problem
@see https://stackoverflow.com/questions/75563597/how-to-access-method-valueof-of-an-unknown-enum-in-scala-3
@see https://stackoverflow.com/questions/75309058/scala-3-macros-how-to-invoke-a-method-obtained-as-a-symbol-in-a-quoted-code-b
@see https://stackoverflow.com/questions/69743692/how-to-create-a-general-method-for-scala-3-enums

for creating a completely new class which would come in handy to automatically add prisms
@see https://github.com/lampepfl/dotty/pull/14124/files#diff-798b5c4e3f8e2a24d2eb4b870bf6b5fa348b3f3aa97d1c43471efd0e5e6e1d61R3602
*/

/** provides a generic interface to the companion object of simple enums where all cases are singletons */
object EnumCompanion {
	def apply[T<:Enum:EnumCompanion]:EnumCompanion[T]	= summon

	inline given of[T<:Enum]:EnumCompanion[T] = ${ ofImpl[T] }

	def ofImpl[T<:Enum](using quotes:Quotes, tpe:Type[T]):Expr[EnumCompanion[T]]	= {
		import quotes.reflect.*

		val companion	= Ref(TypeTree.of[T].symbol.companionModule)

		// TODO dotty use enumLabel instead of productPrefix when it becomes available

		'{
			new EnumCompanion[T] {
				def values:Array[T]				= ${ Select.unique(companion, "values").asExprOf[Array[T]] }

				def fromOrdinal(ordinal:Int):T	= ${ Select.unique(companion, "fromOrdinal").appliedTo('{ordinal}.asTerm).asExprOf[T] }

				def ordinal(element:T):Int		= element.ordinal

				def valueOf(name:String):T		= ${ Select.unique(companion, "valueOf").appliedTo('{name}.asTerm).asExprOf[T] }

				def enumLabel(element:T):String	= element.productPrefix
			}
		}
	}
}

trait EnumCompanion[T] {
	/** as generated by the compiler */
	def values:Array[T]

	/** sames a values, but as a non-empty sequence */
	final def valuesNes:Nes[T]	= Nes.unsafeFromArray(values)

	/** as generated by the compiler */
	def fromOrdinal(ordinal:Int):T

	/** inverse of fromOrdinal, same as element.ordinal */
	def ordinal(element:T):Int

	/** as generated by the compiler */
	def valueOf(name:String):T

	/** inverse of valueOf */
	def enumLabel(element:T):String
}
