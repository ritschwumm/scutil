package scutil.io

import java.io.File
import java.io.InputStream
import java.nio.file.Path
import java.net.URL
import java.net.Proxy
import java.util.Properties

import scutil.core.implicits.*
import scutil.jdk.implicits.*

object PropertiesUtil {
	def empty:Properties	= new Properties

	def loadURL(url:URL, proxy:Option[Proxy] = None):Map[String,String]	=
		loadRawURL(url, proxy).toMap

	def loadFile(path:Path):Map[String,String]	=
		loadRawFile(path).toMap

	def saveFile(path:Path, it:Map[String,String]):Unit	=
		saveRawFile(path, it.toProperties)

	//------------------------------------------------------------------------------

	def loadRawURL(url:URL, proxy:Option[Proxy] = None):Properties	=
		url.withInputStream(proxy) { st =>
			new Properties doto {
				_ load st
			}
		}

	def loadRawFile(path:Path):Properties	=
		path.toFile.withInputStream { st =>
			new Properties doto {
				_ load st
			}
		}

	def saveRawFile(path:Path, it:Properties):Unit	=
		path.toFile.withOutputStream { st =>
			it.store(st, null)
		}

	//------------------------------------------------------------------------------

	// TODO path deprecate and remove
	object file {
		def loadFile(file:File):Map[String,String]	=
			PropertiesUtil.loadFile(file.toPath)

		def saveFile(file:File, it:Map[String,String]):Unit	=
			PropertiesUtil.saveFile(file.toPath, it)

		def loadRawFile(file:File):Properties	=
			PropertiesUtil.loadRawFile(file.toPath)

		def saveRawFile(file:File, it:Properties):Unit	=
			PropertiesUtil.saveRawFile(file.toPath, it)
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
