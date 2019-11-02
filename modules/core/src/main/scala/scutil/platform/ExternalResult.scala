package scutil.platform

final case class ExternalResult(rc:Int, out:Seq[String], err:Seq[String])
