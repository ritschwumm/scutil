package scutil.jcollection

import scutil.jcollection.pimp._

object implicits extends implicits
trait implicits 
		extends	JEnumerationImplicits
		with	JIteratorImplicits
		with	JIterableImplicits
		with	JListImplicits
		with	JMapImplicits
		with	JSetImplicits
		with	PropertiesImplicits
