package scutil.lang

import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

import scutil.lang.implicits._
import scutil.collection.implicits._

/** creates bijections from the apply/unapply methods in a case classes' companion object */
object BijectionGen {
	def apply[T]:AnyRef	= macro BijectionGen.compile[T]
}

private final class BijectionGen(val c:Context) {
	import c.universe._

	def compile[T:c.WeakTypeTag]:Tree	= {
		val selfType:Type	= weakTypeOf[T]
		val selfTypeSymbol	= selfType.typeSymbol

		// TODO use selfTypeSymbol.fullName for print out?

		val out:Either[String,Tree]	=
			if (selfTypeSymbol.asClass.isModuleClass) {
				Right(
					q"""
						_root_.scutil.lang.Bijection[$selfType,_root_.scala.Unit](
							_	=> (),
							_	=> ${selfType.termSymbol}
						)
					"""
				)
			}
			else {
				for {
					_					<-	selfTypeSymbol.isClass && selfTypeSymbol.asClass.isCaseClass	guardEither	s"${selfTypeSymbol.toString} is not a case class"

					companionSymbol		<-	selfTypeSymbol.companion
											.optionNotBy	{ _ == NoSymbol }
											.toRight		(s"unexpected NoSymbol for companion of ${selfTypeSymbol.toString}")

					// TODO use this?
					// companionModule	= companionSymbol.asModule

					companionType		=	companionSymbol.typeSignature

					unapplySymbol		<-	getDeclaration(companionType, "unapply")
					unapplyMethod		=	unapplySymbol.asMethod

					// unapply returns either Option[T] or Option[(T1,T2...)]]
					unapplyReturn		=	unapplyMethod.returnType

					// List(Int)	List((Int, Short))

					// get type parameters out of the Option
					unapplyOuts			<-	unapplyReturn
											.matchOption	{ case t@TypeRef(_, _, args) if t <:< typeOf[Option[Any]]	=> args }
											.toRight		(s"expected unapply TypeRef")

					// Int			(Int,Short)

					// Option has only one type parameter, which may be a product
					unapplySingle		<-	unapplyOuts
											.singleOption
											.toRight	(s"expected unapply Option to contain a single value")

					unapplySignature	<-	unapplySingle
											.matchOption	{ case t @ TypeRef(_, _, _)	=> t }
											.toRight		(s"unexpected unapply return ${unapplySingle.toString}")

					applySymbol			<-	getDeclaration(companionType, "apply")
					applyMethods		=	applySymbol.asMethod.alternatives

					// (method, raw signature)
					applyMethods0		=	for {
												method	<- applyMethods collect { case (method:MethodSymbol) => method }
												params	<- method.paramLists.singleOption.toVector
											}
											yield (
												method,
												params map { _.asTerm.typeSignature }
											)

					// (method, flat signature)
					applyMethods1		=	applyMethods0 filter { case (applyMethod, applySignature) =>
												applySignature.size match {
													case 0	=> false	// should not happen
													case 1	=> equalTypeLists(applySignature, List(unapplySignature))
													case n	=> equalTypeLists(applySignature, unapplySignature.args) &&
															// TODO make sure this doesn't fail for case classes somehow
															unapplySignature <:< typeOf[Product]
												}
											}

					applyTmp			<-	applyMethods1
											.singleOption
											.toRight		(s"expected a single apply method matching unapply's types")

					(applyMethod, applySignature)	= applyTmp
				}
				yield {
					// TODO use fully qualified companion symbol!
					if (applySignature.size == 1)
							q"""
								_root_.scutil.lang.Bijection(
									$companionSymbol.unapply _ andThen {
										case _root_.scala.Some(x)	=> x
										case _root_.scala.None		=> _root_.scala.sys error "case class unapply is expected to be total"
									},
									$companionSymbol.apply _
								)
							"""
					else
							q"""
								_root_.scutil.lang.Bijection(
									$companionSymbol.unapply _ andThen {
										case _root_.scala.Some(x)	=> x
										case _root_.scala.None		=> _root_.scala.sys error "case class unapply is expected to be total"
									},
									($companionSymbol.apply _).tupled
								)
							"""
				}
			}

		out.cata(
			c.abort(c.enclosingPosition, _),
			c untypecheck _
		)
	}

	private def equalTypeLists(a:List[Type], b:List[Type]):Boolean	=
		a.size == b.size
		/*
		// TODO use this again
		&& ((a zip b) forall { case (a, b) => a =:= b })
		*/

	private def getDeclaration(typ:Type, name:String):Either[String,Symbol]	=
		(typ decl TermName(name))
		.optionNotBy	{ _ == NoSymbol }
		.toRight		(s"unexpected NoSymbol for companion declaration ${name} of type ${typ.toString}")
}
