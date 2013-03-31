package scutil.log

import scutil.SourceLocation

case class LogEntry(level:LogLevel, location:SourceLocation, elements:Seq[Any])

