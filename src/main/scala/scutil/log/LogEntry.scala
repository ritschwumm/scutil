package scutil.log

import scutil.lang.SourceLocation

case class LogEntry(level:LogLevel, location:SourceLocation, elements:Seq[Any])

