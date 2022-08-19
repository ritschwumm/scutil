package scutil.platform.os

import java.io.File

import scutil.jdk.implicits.*
import scutil.platform.Platform

object OSX {
	val applicationSupport:File	= Platform.homeDir / "Library" / "Application Support"
	val cache:File				= Platform.homeDir / "Library" / "Cache"
	val logs:File				= Platform.homeDir / "Library" / "Logs"
}
