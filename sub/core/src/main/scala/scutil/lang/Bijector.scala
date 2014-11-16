package scutil.lang

import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context

import scutil.implicits._

/** creates bijections from the apply/unapply methods in a case classes' companion object */
object Bijector {
	def apply[T]:AnyRef	= macro BijectorImpl.compile[T]
}

private final class BijectorImpl(val c:Context) {
	import c.universe._
		
	def compile[T:c.WeakTypeTag]:Tree	= {
		val selfType:Type	= weakTypeOf[T]
		
		val out:Tried[String,Tree]	=
				for {
					companionSymbol	<-
							selfType.typeSymbol.companion
							.preventBy	{ _ == NoSymbol }
							.toWin		(s"unexpected NoSymbol for companion of ${selfType.typeSymbol}")
			
					// TODO use this?
					// companionModule	= companionSymbol.asModule
					
					companionType	= companionSymbol.typeSignature
					
					unapplySymbol	<- getDeclaration(companionType, "unapply")
					unapplyMethod	= unapplySymbol.asMethod
					
					// unapply returns either Option[T] or Option[(T1,T2...)]]
					unapplyReturn	= unapplyMethod.returnType
					
					// List(Int)	List((Int, Short))
					
					// get type parameters out of the Option
					unapplyOuts		<- 
							unapplyReturn 
							.matchOption	{ case t@TypeRef(_, _, args) if t <:< typeOf[Option[Any]]	=> args }	
							.toWin			(s"expected unapply TypeRef")
							
					// Int			(Int,Short)
					
					// Option has only one type parameter, which may be a product
					unapplySingle	<- 
							unapplyOuts
							.singleOption 
							.toWin	(s"expected unapply Option to contain a single value")
							
					unapplySignature	<-
							unapplySingle 
							.matchOption	{ case t @ TypeRef(_, _, _)	=> t }
							.toWin			(s"unexpected unapply return ${unapplySingle}")
							
					applySymbol		<- getDeclaration(companionType, "apply")
					applyMethods	= applySymbol.asMethod.alternatives
					
					// (method, raw signature)
					applyMethods0	=
							for {
								method	<- applyMethods collect { case (method:MethodSymbol) => method }
								params	<- method.paramLists.singleOption
							}
							yield (
								method, 
								params map { _.asTerm.typeSignature }
							)
							
					// (method, flat signature)
					applyMethods1	=
							applyMethods0 filter { case (applyMethod, applySignature) =>
								applySignature.size match {
									case 0	=> false	// should not happen
									case 1	=> equalTypeLists(applySignature, List(unapplySignature))
									case n	=> equalTypeLists(applySignature, unapplySignature.args) &&
											// TODO make sure this doesn't fail for case classes somehow
											unapplySignature <:< typeOf[Product]
								}
							}
							
					applyTmp	<-
							applyMethods1
							.singleOption
							.toWin		(s"expected a single apply method matching unapply's types")
							
					(applyMethod, applySignature)	
								= applyTmp
				}
				yield {
					// TODO use fully qualified companion symbol!
					if (applySignature.size == 1)
							q"""
								_root_.scutil.lang.Bijection(
									$companionSymbol.unapply _ andThen { _.get },
									$companionSymbol.apply _
								)
							"""
					else
							q"""
								_root_.scutil.lang.Bijection(
									$companionSymbol.unapply _ andThen { _.get },
									($companionSymbol.apply _).tupled
								)
							"""
				}
			
		out cata (
			c abort (c.enclosingPosition, _),
			c untypecheck _
		)
	}
	
	private def equalTypeLists(a:List[Type], b:List[Type]):Boolean	=
			a.size == b.size
			/*
			// TODO use this again
			&& ((a zip b) forall { case (a, b) => a =:= b }) 
			*/
			
	private def getDeclaration(typ:Type, name:String):Tried[String,Symbol]	=
			(typ decl TermName(name))
			.preventBy	{ _ == NoSymbol }
			.toWin		(s"unexpected NoSymbol for companion declaration ${name} of type ${typ}")
}
