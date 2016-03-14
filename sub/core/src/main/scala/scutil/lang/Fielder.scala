package scutil.lang

import scala.reflect.macros.blackbox.Context

import scutil.implicits._

object Fielder {
	// NOTE the context bounds makes it possible to pass the type through other methods
	def apply[T:Fielding]:ISeq[String]	= implicitly[Fielding[T]].names
	
	// NOTE application is in Fielding.provide
	// implicit def fielding[T]:Fielding[T]	= macro FielderImpl.apply[T]
	
	/*
	// equivalent to apply, more or less
	
	import scala.reflect.runtime.universe._
	
	def caseClassFieldNames[T:TypeTag]:Option[ISeq[String]]	=
			for {
				primaryCtor	<- (typeOf[T].declarations filter { _.isMethod } map { _.asMethod } filter { _.isPrimaryConstructor }).singleOption
				paramNames	<- primaryCtor.paramss.singleOption
			}
			yield paramNames map { _.name.decoded }
	*/
}

private final class FielderImpl(val c:Context) {
	import c.universe._
		
	def compile[T:c.WeakTypeTag]:c.Tree	= {
		val selfType:Type	= weakTypeOf[T]

		val primaryCtorOpt	=
				(selfType.decls filter { _.isMethod } map { _.asMethod } filter { _.isPrimaryConstructor }).singleOption
		
		val names:Tried[String,Tree]	=
				for {
					primaryCtor		<- primaryCtorOpt						toWin s"primary constructor not found in ${selfType}"
					paramNames		<- primaryCtor.paramLists.singleOption	toWin s"primary constructor has multiple parameter lists in ${selfType}"
					decodedNames	= paramNames map { _.name.decodedName.toString }
				}
				yield {
					q"_root_.scutil.lang.Fielding[$selfType](scala.collection.immutable.Vector(..$decodedNames))"
				}
				
		names cata (
			c abort (c.enclosingPosition, _),
			c untypecheck _
		)
	}
}
