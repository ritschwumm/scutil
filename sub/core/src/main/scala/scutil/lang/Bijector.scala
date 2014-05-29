package scutil.lang

import scala.language.experimental.macros

import scala.reflect.macros.whitebox.Context
import scala.reflect.runtime.universe._

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
			
					companionType	= companionSymbol.typeSignature
					
					unapplySymbol	<- getDeclaration(companionType, "unapply")
					unapplyMethod	= unapplySymbol.asMethod
					unapplyReturn	= unapplyMethod.returnType
					
					// List(Int)	List((Int, Short))
					unapplyRef		<- 
							unapplyReturn 
							.matchOption	{ case TypeRef(_, _, args) => args }	
							.toWin			(s"expected unapply TypeRef")
					// Int			(Int,Short)
					unapplySingle	<- 
							unapplyRef
							.singleOption 
							.toWin	(s"expected unapply Option to return a single value")
							
					unapplySignature	<-
							unapplySingle match {
								case t @ TypeRef(_, _, _)	=> Win(t)
								case x						=> Fail(s"unexpected unapply return ${x}")
							}
							
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
									case 1	=> applySignature == List(unapplySignature)
									case n	=> applySignature == unapplySignature.args &&
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
							
					_			<- 
							(applyMethod.paramLists.size == 1) 
							.trueWin	(s"expected apply to have a single parameter list")
				}
				yield {
					q"""
						scutil.lang.Bijection(
							(it:$selfType) => $companionSymbol.unapply(it).get, 
							${
								if (applySignature.size == 1) {
									q"(it:$unapplySingle) => $companionSymbol.apply(it)"
								}
								else {
									val its	= 1 to applySignature.size map { i => q"it.${TermName("_"+i)}" }
									q"(it:$unapplySingle) => $companionSymbol.apply(..$its)"
								}
							}
						)
					"""
				}
			
		out cata (
			c abort (c.enclosingPosition, _),
			c untypecheck _
		)
	}
			
	private def getDeclaration(typ:Type, name:String):Tried[String,Symbol]	=
			(typ decl TermName(name))
			.preventBy	{ _ == NoSymbol }
			.toWin		(s"unexpected NoSymbol for companion declaration ${name} of type ${typ}")
}
