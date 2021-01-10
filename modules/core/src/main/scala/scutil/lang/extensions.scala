package scutil.lang

import scutil.lang.extension._

object extensions extends extensions

trait extensions
	extends	AnyImplicits
	with	AnyRefImplicits
	with	AtomicReferenceImplicits
	with	BooleanImplicits
	with	ByteArrayImplicits
	with	CharsetImplicits
	with	ClassImplicits
	with	ExceptionCatchImplicits
	with	EitherImplicits
	with	Function0Implicits
	with	Function1Implicits
	with	Function2Implicits
	with	FutureImplicits
	with	OptionImplicits
	with	PEndoImplicits
	with	PFunctionImplicits
	with	PartialFunctionImplicits
	with	PredicateImplicits
	with	StringImplicits
	with	ThrowableImplicits
	with	TryImplicits
