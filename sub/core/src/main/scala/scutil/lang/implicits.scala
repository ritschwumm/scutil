package scutil.lang

import scutil.lang.pimp._

object implicits extends implicits
trait implicits 
		extends	AnyImplicits 
		with	AnyRefImplicits
		with	BooleanImplicits
		with	ClassImplicits
		with	ExceptionCatchImplicits
		with	EitherImplicits
		with	Function0Implicits
		with	Function1Implicits
		with	Function2Implicits
		with	HomogenousPairImplicits
		with	OptionImplicits
		with	PEndoImplicits
		with	PFunctionImplicits
		with	PairImplicits
		with	PartialFunctionImplicits
		with	PredicateImplicits
		with	StringContextImplicits
		with	ThrowableImplicits
		with	TryImplicits
