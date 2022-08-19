package scutil.platform.os

import java.io.File
import java.nio.file.Path

import scutil.jdk.implicits.*
import scutil.platform.Platform

object OSX {
	val applicationSupport:Path	= Platform.homeDir / "Library" / "Application Support"
	val cache:Path				= Platform.homeDir / "Library" / "Cache"
	val logs:Path				= Platform.homeDir / "Library" / "Logs"
}
