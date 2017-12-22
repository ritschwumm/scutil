package scutil.io

import java.io._
import java.nio.file._

import scutil.lang.tc._

object instances extends instances

trait instances {
	implicit def FileShow:Show[File]	= Show instance (_.getPath)
	implicit def PathShow:Show[Path]	= Show.toStringInstance
}
