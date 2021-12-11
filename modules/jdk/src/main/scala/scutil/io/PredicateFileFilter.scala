package scutil.io.extension

import java.io.*

import scutil.lang.*

final class PredicateFileFilter(predicate:Predicate[File]) extends FileFilter {
	def accept(file:File):Boolean	= predicate(file)
}
