package scutil.io

import scutil.io.pimp._

object implicits extends implicits
trait implicits
		extends	FileImplicits
		with	InputStreamImplicits
		with	OutputStreamImplicits
		with	ReaderImplicits
		with	URLImplicits
