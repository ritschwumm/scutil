package scutil.jcollection

import scutil.jcollection.pimp._

object extensions extends extensions
trait extensions
		extends	ConcurrentLinkedQueueImplicits
		with	JEnumerationImplicits
		with	JIteratorImplicits
		with	JIterableImplicits
		with	JListImplicits
		with	JMapImplicits
		with	JOptionalImplicits
		with	JSetImplicits
		with	PropertiesImplicits
		with	SeqJCollectionSyntaxImplicits
		with	IterableJCollectionSyntaxImplicits
		with	IteratorJCollectionSyntaxImplicits
		with	MapJCollectionSyntaxImplicits
		with	SetJCollectionSyntaxImplicits
