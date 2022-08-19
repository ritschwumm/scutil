package scutil.platform.os

import java.io.File
import java.nio.file.Path

import scutil.jdk.implicits.*
import scutil.platform.Platform

object Linux {
	def dotFile(name:String):Path	= Platform.homeDir / s".$name"
	val dotLocal:Path				= Platform.homeDir / ".local"
	val dotLocalShare:Path			= dotLocal / "share"
	val dotCache:Path				= Platform.homeDir / ".cache"
}
