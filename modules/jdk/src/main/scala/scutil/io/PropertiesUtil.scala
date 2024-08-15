package scutil.io

import java.io.InputStream
import java.nio.file.Path
import java.net.URL
import java.net.Proxy
import java.util.Properties

import scutil.core.implicits.*
import scutil.jdk.implicits.*

object PropertiesUtil {
	def empty:Properties	= new Properties

	def loadURL(url:URL, proxy:Option[Proxy]):Map[String,String]	=
		loadRawURL(url, proxy).toMap

	def loadFile(path:Path):Map[String,String]	=
		loadRawFile(path).toMap

	def saveFile(path:Path, it:Map[String,String]):Unit	=
		saveRawFile(path, it.toProperties)

	//------------------------------------------------------------------------------

	def loadRawURL(url:URL, proxy:Option[Proxy]):Properties	=
		url.withInputStream(proxy) { st =>
			new Properties doto {
				_.load(st)
			}
		}

	def loadRawFile(path:Path):Properties	=
		MoreFiles.withInputStream(path) { st =>
			new Properties doto {
				_.load(st)
			}
		}

	def saveRawFile(path:Path, it:Properties):Unit	=
		MoreFiles.withOutputStream(path) { st =>
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
		p.load(ist)
		out
	}
}
