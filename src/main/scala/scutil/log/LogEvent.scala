package scutil.log

import scutil.lang.SourceLocation

final case class LogEvent(level:LogLevel, elements:Seq[Any], location:SourceLocation)
