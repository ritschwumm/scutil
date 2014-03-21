package scutil.lang

import scala.language.dynamics
import scala.language.experimental.macros

import scala.reflect.macros.Context

import scutil.implicits._

/** creates lens instances for a case classes' fields */
object Lenser {
	def apply[T]	= new Lenser[T]
}

final class Lenser[T] extends Dynamic {
	/*
	def selectDynamic(propName:String)	= macro LenserImpl.selectDynamic[T]
	def applyDynamic(propName:String)()	= macro LenserImpl.applyDynamic[T]
	*/
	
	def selectDynamic(propName:String)	= macro LenserImpl.selectDynamic[T]
}

private object LenserImpl {
	/*
	def selectDynamic[T:c.WeakTypeTag](c:Context)(propName:c.Expr[String])	=
	def applyDynamic[T:c.WeakTypeTag](c:Context)(propName:c.Expr[String])()	=
	*/
	
	def selectDynamic[T:c1.WeakTypeTag](c1:Context)(propName:c1.Expr[String]):c1.Expr[Any]	=
			new LenserImpl { val c:c1.type = c1 } compile propName
}

private abstract class LenserImpl extends MacroHelper {
	val c:Context
	import c.universe._
		
	//------------------------------------------------------------------------------
	
	def compile[T:c.WeakTypeTag](propName:c.Expr[String]):c.Expr[Any]	= {
		val targetType	= c.weakTypeOf[T]
		
		val out:Tried[String,Apply]	=
				for {
					name	<- 
							propName.tree										matchOption 
							{ case Literal(Constant(name:String))	=> name }	toWin
							s"unexpected propName: ${propName.tree}"
					containerTpe	= c.weakTypeOf[T]
					member	<-
							containerTpe		member 
							newTermName(name)	guardBy 
							{ _ != NoSymbol }	toWin 
							s"value ${name} is not a member of ${containerTpe}"
					valueTpe	<-
							member									typeSignatureIn
							containerTpe							matchOption
							{ case NullaryMethodType(tpe) => tpe }	toWin
							s"member ${name} of ${containerTpe} is not a field"
				}
				yield mkLens("c$", containerTpe, "v$", valueTpe, name)
				
		result(out)
	}
	
	def mkLens(containerName:TermName, containerType:Type, valueName:TermName, valueType:Type, fieldName:TermName):Apply	=
			Apply(
				TypeApply(
					multiSelect("scutil", "lang", "TLens", "create"),
					// NOTE doesn't work - why?
					// Select(reify(scutil.lang.TLens).tree, "create":TermName),
					List(
						TypeTree(containerType),
						TypeTree(valueType)
					)
				),
				List(
					mkGetFunc(containerName, containerType, fieldName),
					mkSetFunc(containerName, containerType, valueName, valueType, fieldName)
				)
			)
			
	def mkGetFunc(containerName:TermName, containerType:Type, fieldName:TermName):Function	=
			Function(
				List(
					mkParam(containerName, containerType)
				),
				mkAccess(containerName, fieldName)
			)
			
	def mkSetFunc(containerName:TermName, containerType:Type, valueName:TermName, valueType:Type, fieldName:TermName):Function	=
			Function(
				List(
					mkParam(containerName,	containerType),
					mkParam(valueName,		valueType)
				),
				Apply(
					mkAccess(containerName, "copy"),
					List(
						AssignOrNamedArg(
							Ident(fieldName),
							Ident(valueName)
						)
					 )
				)
			)
}
