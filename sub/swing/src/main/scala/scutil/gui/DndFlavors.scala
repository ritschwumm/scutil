package scutil.gui

import java.awt.datatransfer.DataFlavor

object DndFlavors {
	val javaFileList	= DataFlavor.javaFileListFlavor
	
	// @see http://forums.sun.com/thread.jspa?threadID=206311&messageID=3751645
	val	uriList			= new DataFlavor("text/uri-list;class=java.lang.String")
	val url				= new DataFlavor("application/x-java-url; class=java.net.URL")
	
	// NOTE ;filename=foobar does not work
	val binary			= new DataFlavor("application/octet-stream;class=java.io.InputStream", "raw binary file")
}
