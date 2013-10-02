package scutil.log

import scutil.lang.SourceLocation

final case class LogEvent(level:LogLevel, location:SourceLocation, elements:Seq[Any])
