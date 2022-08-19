package scutil.platform.os

import java.io.File

import scutil.jdk.implicits.*
import scutil.platform.Platform

object Windows {
	// NOTE In XP, Roaming was <user>\Application Data and Local was <user>\Local Settings\Application Data
	val localAppData:Option[File]	= Platform env "LOCALAPPDATA"	map (new File(_))
	val appData:Option[File]		= Platform env "APPDATA"		map (new File(_))
}
