package scutil.io.extension

import java.io._

import scutil.lang._

final class PredicateFileFilter(predicate:Predicate[File]) extends FileFilter {
	def accept(file:File):Boolean	= predicate(file)
}
