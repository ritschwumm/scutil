package scutil.log

import scutil.lang.SourceLocation

final case class LogEntry(level:LogLevel, location:SourceLocation, elements:Seq[Any])

