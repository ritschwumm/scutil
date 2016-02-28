package scutil.lang

import scutil.lang.pimp._

object implicits extends implicits
trait implicits
		extends	disposables
		with	AnyImplicits
		with	AnyRefImplicits
		with	BooleanImplicits
		with	ClassImplicits
		with	ClassLoaderImplicits
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
		with	RandomImplicits
		with	StatefulImplicits
		with	StringContextImplicits
		with	ThrowableImplicits
		with	TryImplicits
