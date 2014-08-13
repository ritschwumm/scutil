package scutil.platform

import scutil.lang.ISeq

final case class ExternalResult(rc:Int, out:ISeq[String], err:ISeq[String])
