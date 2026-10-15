package scutil.lang

import scala.annotation.nowarn
import scala.language.dynamics
import scala.deriving.*
import scala.quoted.*

/** creates prism instances for an enum's variants */
object PrismGen {
	def apply[T]	= new PrismGen[T]
}

// NOTE dotty Selectable is not good enough for this (?)
final class PrismGen[T] extends Dynamic {
	inline transparent def selectDynamic[N <: String & Singleton](inline variantName:N):Any	= ${ PrismGenImpl.selectImpl[T, N]('variantName) }
}

/*
without macros @see https://rockthejvm.com/articles/scala-3-type-level-programming
for getting parameters from the selectDyanamic into the macro @see https://inoio.de/blog/2024/07/15/scala3-macros-part2/
tuple type helpers @see https://docs.scala-lang.org/scala3/guides/macros/faq.html
*/
object PrismGenImpl {
	// NOTE needs a transparent call to make the return type flexible
	@nowarn("msg=unused pattern variable")
	def selectImpl[T:Type, N<:String&Singleton:Type](variantNameExpr:Expr[N])(using quotes:Quotes):Expr[Any] = {
		import quotes.reflect.*

		val sumTypeRepr:TypeRepr	= TypeRepr.of[T].dealias
		val sumTypeName:String		= sumTypeRepr.typeSymbol.fullName

		val variantName:String		= variantNameExpr.valueOrAbort

		val mirror:Expr[Mirror.Of[T]]	=
			Expr.summon[Mirror.Of[T]].getOrElse {
				report.errorAndAbort(
					s"cannot find a mirror for type ${sumTypeName}",
					// put the error on the name
					variantNameExpr.asTerm.pos
				)
			}

		mirror match {
			case '{
				// extract the exact types of types ans labels
				type types <: Tuple
				type labels <: Tuple
				$m:Mirror.SumOf[T] { type MirroredElemTypes	= `types`; type MirroredElemLabels	= `labels` }
			} =>
				/*
				// NOTE example code: we can get at the labels as strings like this

				// resembling constValueTuple[labels] which does not work here
				val labelValues	=
					Type.valueOfTuple[labels].getOrElse {
						report.errorAndAbort(
							s"cannot find variant labels for ${sumTypeName}",
							variantNameExpr.asTerm.pos
						)
					}

				// we know these are strings, but MirroredElemLabels is a generic tuple so scala doesn't know
				val labelNames:IArray[String]	= labelValues.toIArray.map(_.toString)

				val index	= labelNames.indexOf(variantName)
				if (index < 0) {
					report.errorAndAbort(
						s"cannot find variant ${variantName} for sum type ${sumTypeName}",
						variantNameExpr.asTerm.pos
					)
				}
				*/

				type Variant = Lookup[labels, types, N]

				// ensure the variant is actually a subtype: the code above does not guarantee that but resolves to some useless type if it is not
				// simplification is useful for debug output but nothing else (?)
				val variantTypeRepr	= TypeRepr.of[Variant].simplified

				/*
				// NOTE example code for printing what we have

				println(Type.show(using variantTypeRepr.asType))
				println(TypeTree.of(using variantTypeRepr.asType).show(using Printer.TreeShortCode))

				println(Type.show[Variant])
				println(TypeTree.of[Variant].show(using Printer.TreeShortCode))
				*/

				val isValidSubtype	= variantTypeRepr <:< sumTypeRepr
				if !isValidSubtype then {
					report.errorAndAbort(
						s"cannot find variant ${variantName} for sum type ${sumTypeName}",
						// put the error on the name
						variantNameExpr.asTerm.pos
					)
				}

				// from the mirror we cannot prove that Variant is a subtype of T, but for the prism we have to
				type Subtype	= Variant & T

				'{ Prism.subType[T, Subtype] }
		}
	}

	// treat Labels and Names as an associative list and find the name for a given label
	private type Lookup[Labels <: Tuple, Names <: Tuple, Label <: Singleton]	=
		(Labels, Names) match {
			case (Label *: ls,	n *: ns)	=> n
			case (_ *: ls,		_ *: ns)	=> Lookup[ls, ns, Label]
		}

	/*
	// treat Labels and Names as an associative list and find the name for a given label
	// NOTE resolves to ValueOf[Head[EmptyTuple]] if the name does not designate a variant
	private type Lookup[Labels <: Tuple, Names <: Tuple, Label <: Singleton]	=
		ValueOf[
			Tuple.Head[
				Tuple.Filter[
					HardZip[Labels, Names],
					HasLabel[Label]
				]
			]
		]

	private type ValueOf[Assoc]	=
		Assoc match {
			case (_, value)	=> value
		}

	// like Tuple.Zip but fails for tuples with different sizes
	private type HardZip[T1 <: Tuple, T2 <: Tuple] <: Tuple	=
		(T1, T2) match {
			case (h1 *: t1,		h2 *: t2)	=> (h1, h2) *: HardZip[t1, t2]
			case (EmptyTuple,	EmptyTuple)	=> EmptyTuple
		}

	private type HasLabel[Label]	=
		[Assoc] =>> Assoc match {
			case (Label, _)	=> true
			case _			=> false
		}
	*/
}
