package scutil.lang

import scutil.lang.syntaxes._

object syntax extends syntax

trait syntax
	extends ReleaseableSyntax
	with	TupleNApplySyntax
