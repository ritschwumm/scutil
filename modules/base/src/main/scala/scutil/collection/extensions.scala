package scutil.collection

import scutil.collection.pimp._

object extensions extends extensions
trait extensions
	extends	IteratorImplicits
	with	SeqImplicits
	with	ListImplicits
	with	MapImplicits
	with	QueueImplicits
	with	SetImplicits
	with	IterableImplicits
