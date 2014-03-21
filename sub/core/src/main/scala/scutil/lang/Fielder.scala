package scutil.lang

import language.experimental.macros
import scala.reflect.macros.Context

import reflect.runtime.universe.TypeTag

import scutil.implicits._
	
object Fielder {
	// NOTE the context bounds makes it possible to pass the type through other methods
	def apply[T:Fielding]:Seq[String]	= implicitly[Fielding[T]].names
	
	// NOTE application is in Fielding.provide
	// implicit def fielding[T]:Fielding[T]	= macro FielderImpl.apply[T]
	
	/*
	// import scala.reflect.runtime.universe._
	
	def caseClassFieldNames[T:TypeTag]:Option[Seq[String]]	=
			for {
				primaryCtor	<- (typeOf[T].declarations filter { _.isMethod } map { _.asMethod } filter { _.isPrimaryConstructor }).singleOption
				paramNames	<- primaryCtor.paramss.singleOption
			}
			yield paramNames map { _.name.decoded }
	*/
}

private object FielderImpl {
	def apply[T:c1.WeakTypeTag](c1:Context):c1.Expr[Fielding[T]]	=
			(new FielderImpl { val c:c1.type = c1 }).compile
}

private abstract class FielderImpl extends MacroHelper {
	val c:Context
	import c.universe._
		
	//------------------------------------------------------------------------------
	
	def compile[T:c.WeakTypeTag]:c.Expr[Fielding[T]]	= {
		val selfType:Type	= weakTypeOf[T]
    
		val primaryCtorOpt	=
				(selfType.declarations filter { _.isMethod } map { _.asMethod } filter { _.isPrimaryConstructor }).singleOption
		
		val names:Tried[String,Tree]	=
				for {
					primaryCtor		<- primaryCtorOpt					toWin s"primary constructor not found in ${selfType}"
					paramNames		<- primaryCtor.paramss.singleOption	toWin s"primary constructor has multiple parameter lists in ${selfType}"
					decodedNames	= paramNames map { _.name.decoded }
				}
				yield {
					Apply(
						TypeApply(
							multiSelect("scutil", "lang", "Fielding"),
							List(
								TypeTree(selfType)
							)
						),
						List(
							Apply(
								multiSelect("scala", "collection", "immutable", "Vector", "apply"),
								decodedNames map { it => (c literal it).tree }
							)
						)
					)
				}
				
		result[Fielding[T]](names)
	}
}
