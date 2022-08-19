package scutil.platform.os

import java.io.File
import java.nio.file.Path

import scutil.jdk.implicits.*
import scutil.platform.Platform

object Windows {
	// NOTE In XP, Roaming was <user>\Application Data and Local was <user>\Local Settings\Application Data
	val localAppData:Option[Path]	= Platform env "LOCALAPPDATA"	map (Path.of(_))
	val appData:Option[Path]		= Platform env "APPDATA"		map (Path.of(_))
}
