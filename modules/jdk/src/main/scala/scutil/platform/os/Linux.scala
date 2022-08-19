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

	// TODO path deprecate and remove
	object file {
		def dotFile(name:String):File	= Linux.dotFile(name).toFile
		val dotLocal:File				= Linux.dotLocal.toFile
		val dotLocalShare:File			= Linux.dotLocalShare.toFile
		val dotCache:File				= Linux.dotCache.toFile
	}
}
