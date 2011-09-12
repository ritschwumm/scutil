package scutil.validation

import scutil.log._
import scutil.ext.OptionImplicits._

import ValidationInstances._

object SimpleSafety extends Logging {
	type Safe[T]	= Validated[NonEmptyList[String],T]
	
	def valid[T](value:T):Safe[T]	= 
			Validated valid	value
			
	def invalid[T](problem:String):Safe[T]	= 
			Validated invalid (NonEmptyList unital problem)
			
	def invalidMulti[T](problems:Seq[String]):Safe[T]	=
			Validated invalid (NonEmptyList option problems getOrError "no empty sequence allowed here")
			
	// BETTER use an implicit on Safe instead?
	// BETTER make warning level configurable?
	/** log all problems in a Safe[T] to ERROR */
	def logProblems[T](safe:Safe[T]):Safe[T] = {
		safe.invalid foreach { it =>  ERROR(it.toList:_*) }
		safe
	}
	
	/** allow turning an Option[T] into a Safe[T] by supplying a problem text */
	implicit def SafeOption[T](delegate:Option[T]) = new {
		def elseInvalid (problem: =>String):Safe[T]		= Validated whenSome (delegate, NonEmptyList(problem))
	}
	
	/** allow turning a Boolean into a Safe[Unit] by supplying a problem text for false */
	implicit def SafeBoolean(delegate:Boolean) = new {
		def elseInvalid (problem: =>String):Safe[Unit]	= Validated when (delegate, (), NonEmptyList(problem))
	}
	
	/** allow turning an Either[String,T] into a Safe[T] */
	implicit def SafeEither[T](delegate:Either[String,T]) = new {
		def whenRight:Safe[T]	=	Validated whenRight (delegate.left map NonEmptyList.unital)
	}
}
