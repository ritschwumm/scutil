package scutil.io

import scutil.io.pimp._

object extensions extends extensions
trait extensions
		extends	FileImplicits
		with	PathImplicits
		with	InputStreamImplicits
		with	OutputStreamImplicits
		with	ReaderImplicits
		with	WriterImplicits
		with	URLImplicits
