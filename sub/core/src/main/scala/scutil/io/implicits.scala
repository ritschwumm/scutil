package scutil.io

import scutil.io.pimp._

object implicits extends implicits
trait implicits
		extends	disposables
		with	FileImplicits
		with	InputStreamImplicits
		with	OutputStreamImplicits
		with	ReaderImplicits
		with	URLImplicits
