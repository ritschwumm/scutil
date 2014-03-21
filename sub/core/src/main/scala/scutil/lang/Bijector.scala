package scutil.lang

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.reflect.runtime.universe._

import scutil.implicits._

/** creates bijections from the apply/unapply methods in a case classes' companion object */
object Bijector {
	def apply[T]	= macro BijectorImpl.apply[T]
}

private object BijectorImpl {
	def apply[T:c1.WeakTypeTag](c1:Context):c1.Expr[Any]	=
			(new BijectorImpl { val c:c1.type = c1 }).compile
}

private abstract class BijectorImpl extends MacroHelper {
	val c:Context
	import c.universe._
		
	//------------------------------------------------------------------------------
	
	def compile[T:c.WeakTypeTag]:c.Expr[Any]	= {
		val selfType:Type	= weakTypeOf[T]
		
		val out:Tried[String,Apply]	=
				for {
					companionSymbol	<- getCompanion(selfType.typeSymbol)
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
								params	<- method.paramss.singleOption
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
							(applyMethod.paramss.size == 1) 
							.trueWin	(s"expected apply to have a single parameter list")
				}
				yield mkBijection(
					mkWriteFunc(companionSymbol, selfType), 
					mkReadFunc(companionSymbol, applySignature, unapplySingle)
				)
			
		result(out)
	}
	
	def mkBijection(writeFunc:Function, readFunc:Function):Apply =
			// BETTER use typeApply?
			Apply(
				multiSelect("scutil", "lang", "Bijection", "apply"),
				// NOTE does work at first, but lead to "value not found: Bijection" later - why?
				// Select(reify(scutil.lang.Bijection).tree, "apply":TermName),
				List(
					writeFunc,
					readFunc
				)
			)
			
	// write	unapply		call get	T=>(...)
	def mkWriteFunc(companionSymbol:Symbol, selfType:Type):Function	=
			Function(
				List(
					mkParam("it", selfType)
				),
				Select(
					Apply(
						multiSelect(companionSymbol, "unapply"),
						List(stringIdent("it"))
					),
					newTermName("get")
				)
			)
			
	// read		apply		tuple input	(...)=>T
	def mkReadFunc(companionSymbol:Symbol, applySignature:List[Type], unapplySingle:Type):Function	=
			Function(
				List(
					mkParam("it", unapplySingle)
				),
				Apply(
					multiSelect(companionSymbol, "apply"),
					if (applySignature.size == 1) {
						List(stringIdent("it"))
					}
					else {
						(1 to applySignature.size).toList map { i =>
							multiSelect("it", "_"+i)
						}
					}
				)
			)
			
	def getCompanion(symbol:Symbol)	=
			symbol.companionSymbol	preventBy
			{ _ == NoSymbol }		toWin
			s"unexpected NoSymbol for companion of ${symbol}"
	
	def getDeclaration(typ:Type, name:String):Tried[String,Symbol]	=
			typ					declaration
			newTermName(name)	preventBy 
			{ _ == NoSymbol }	toWin 
			s"unexpected NoSymbol for companion declaration ${name} of type ${typ}"
}
