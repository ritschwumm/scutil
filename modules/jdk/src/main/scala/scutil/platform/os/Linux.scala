package scutil.platform.os

import java.io.File

import scutil.jdk.implicits.*
import scutil.platform.Platform

object Linux {
	def dotFile(name:String):File	= Platform.homeDir / s".$name"
	val dotLocal:File				= Platform.homeDir / ".local"
	val dotLocalShare:File			= dotLocal / "share"
	val dotCache:File				= Platform.homeDir / ".cache"
}
