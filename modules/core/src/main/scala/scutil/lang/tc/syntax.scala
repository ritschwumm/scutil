package scutil.lang.tc

object syntax extends syntax

trait syntax
	extends	FunctorSyntax
	with	ApplicativeSyntax
	with	MonadSyntax
	with	SemigroupSyntax
	with	MonoidSyntax
	with	ShowSyntax
	with	TraversedSyntax
	with	ApplicativeTupleNSyntax
