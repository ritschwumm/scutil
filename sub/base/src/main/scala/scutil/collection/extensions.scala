package scutil.collection

import scutil.collection.pimp._

object extensions extends extensions
trait extensions
		extends	IterableImplicits
		with	IteratorImplicits
		with	ISeqImplicits
		with	ListImplicits
		with	MapImplicits
		with	QueueImplicits
		with	SetImplicits
		with	TraversableImplicits
		with	TraversableOnceImplicits
