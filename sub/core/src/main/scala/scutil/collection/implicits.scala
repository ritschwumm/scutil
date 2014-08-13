package scutil.collection

import scutil.collection.pimp._

object implicits extends implicits
trait implicits 
		extends	disposables
		with	IteratorImplicits
		with	ISeqImplicits
		with	ListImplicits
		with	MapImplicits
		with	QueueImplicits
		with	SetImplicits
		with	TraversableImplicits
		with	TraversableOnceImplicits
