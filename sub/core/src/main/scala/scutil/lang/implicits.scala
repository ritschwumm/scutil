package scutil.lang

import scutil.lang.pimp._

object implicits extends implicits
trait implicits 
		extends	disposables
		with	AnyImplicits 
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
		with	PFunctionImplicits
		with	PairImplicits
		with	PartialFunctionImplicits
		with	PredicateImplicits
		with	ThrowableImplicits
		with	TryImplicits