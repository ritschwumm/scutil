package scutil.io

import java.io.File
import java.net.URL
import java.util.Properties

import scala.collection.JavaConverters._

import scutil.pimp.AnyImplicits._
import scutil.pimp.URLImplicits._
import scutil.pimp.FileImplicits._

object PropertiesUtil {
	def loadURL(url:URL):Map[String,String]	=
			toScala(loadRawURL(url))
		
	def loadFile(file:File):Map[String,String]	=
			toScala(loadRawFile(file))
	
	def saveFile(file:File, it:Map[String,String]):Unit	=
			saveRawFile(file, fromScala(it))
			
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
			
	//------------------------------------------------------------------------------
	
	def toScala(it:Properties):Map[String,String]	=
			it.asScala.toMap
	
	def fromScala(it:Map[String,String]):Properties	=
			new Properties doto { out =>
				it foreach (out.put _).tupled
			}
}
