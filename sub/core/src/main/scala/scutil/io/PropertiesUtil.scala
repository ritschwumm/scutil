package scutil.io

import java.io.File
import java.net.URL
import java.util.Properties

import scutil.implicits._

object PropertiesUtil {
	def empty:Properties	= new Properties
	
	def loadURL(url:URL):Map[String,String]	=
			loadRawURL(url).toMap
		
	def loadFile(file:File):Map[String,String]	=
			loadRawFile(file).toMap
	
	def saveFile(file:File, it:Map[String,String]):Unit	=
			saveRawFile(file, it.toProperties)
			
	//------------------------------------------------------------------------------
	
	def loadRawURL(url:URL):Properties	=
			url withInputStream { st =>
				new Properties doto {
					_ load st
				}
			}
		
	def loadRawFile(file:File):Properties	=
			file withInputStream { st =>
				new Properties doto {
					_ load st
				}
			}
	
	def saveRawFile(file:File, it:Properties):Unit	=
			file withOutputStream { st =>
				it store (st, null)
			}
}
