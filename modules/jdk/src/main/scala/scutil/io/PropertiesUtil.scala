package scutil.io

import java.io.File
import java.io.InputStream
import java.net.URL
import java.net.Proxy
import java.util.Properties

import scutil.core.implicits.*
import scutil.jdk.implicits.*

object PropertiesUtil {
	def empty:Properties	= new Properties

	def loadURL(url:URL, proxy:Option[Proxy] = None):Map[String,String]	=
		loadRawURL(url, proxy).toMap

	def loadFile(file:File):Map[String,String]	=
		loadRawFile(file).toMap

	def saveFile(file:File, it:Map[String,String]):Unit	=
		saveRawFile(file, it.toProperties)

	//------------------------------------------------------------------------------

	def loadRawURL(url:URL, proxy:Option[Proxy] = None):Properties	=
		(url withInputStream proxy) { st =>
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
			it.store(st, null)
		}

	//------------------------------------------------------------------------------

	def loadOrdered(ist:InputStream):Seq[(String,String)]	= {
		var out	= Vector.empty[(String,String)]
		val p	= new Properties() {
			@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
			override def put(key:AnyRef, value:AnyRef):AnyRef	= {
				out	:+= (key.asInstanceOf[String] -> value.asInstanceOf[String])
				super.put(key, value)
			}
		}
		p load ist
		out
	}
}
