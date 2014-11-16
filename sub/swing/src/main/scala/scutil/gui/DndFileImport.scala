package scutil.gui

import java.io.{ File, IOException }
import java.util.{ List => JList }
import java.net.{ URI, URL }
import java.awt.datatransfer.DataFlavor
import javax.swing.{ JComponent, TransferHandler }
import javax.swing.TransferHandler.TransferSupport

import scutil.lang._
import scutil.implicits._
import scutil.gui.implicits._
import scutil.geom.IntPoint

object DndFileImport {
	def install(target:JComponent, consumer:PFunction[IntPoint,Effect[Validated[Exception,Nes[File]]]]):Disposable	= {
		target setTransferHandler new FileTransferHandler(consumer)
		
		disposable {
			target setTransferHandler null
		}
	}
	
	private val importable:Set[DataFlavor]	=
			Set(
				DndFlavors.javaFileList,
				DndFlavors.uriList,
				DndFlavors.url
			)
			
	private final class FileTransferHandler(consumer:PFunction[IntPoint,Effect[Validated[Exception,Nes[File]]]]) extends TransferHandler {
		override def canImport(support:TransferSupport):Boolean =
				importEffect(support).isDefined
	
		override def importData(support:TransferSupport):Boolean =
				importEffect(support) cata (
					// wrong format or bad place
					false,
					effect => {
						//@see http://www.davidgrant.ca/drag_drop_from_linux_kde_gnome_file_managers_konqueror_nautilus_to_java_applications
						val extracted:Validated[Exception,Nes[File]]	=
								// linux
								extractFileList[String]		(support, DndFlavors.uriList,		filesFromURIList)	orElse
								// windows / osx
								extractFileList[JList[File]](support, DndFlavors.javaFileList,	filesFromJList)		orElse
								// not actually used?
								extractFileList[URL]		(support, DndFlavors.url,			filesFromURL)		getOrElse
								// unknown format
								(Validated bad badMessage("unexpected transfer flavor"))
						(extracted doto effect).isGood
					}
				)
				
		//------------------------------------------------------------------------------
				
		private def importEffect(support:TransferSupport):Option[Effect[Validated[Exception,Nes[File]]]] =
				supportsFormat(support)	flatGuard consumer(dropIntPoint(support))
				
		private def supportsFormat(support:TransferSupport):Boolean =
				support.getDataFlavors.toSet containsAny DndFileImport.importable
			
		private def dropIntPoint(support:TransferSupport):IntPoint	=
				support.getDropLocation.getDropPoint.toIntPoint
	
		private def extractFileList[T](support:TransferSupport, flavor:DataFlavor, extractor:T=>Validated[Exception,Nes[File]]):Option[Validated[Exception,Nes[File]]]	=
				support isDataFlavorSupported flavor guard {
					extractTransferData[T](support, flavor) mapFail Nes.single into Validated.fromTried flatMap extractor
				}
				
		private def extractTransferData[T](support:TransferSupport, flavor:DataFlavor):Tried[Exception,T]	=
				Catch.exception in (support.getTransferable getTransferData flavor).asInstanceOf[T]
				
		private def filesFromURIList(uriList:String):Validated[Exception,Nes[File]]	=
				uriList
				.splitAroundString	("\r\n")
				.filterNot			{ _.isEmpty			}
				.filterNot			{ _ startsWith "#"	}
				.map				(fileFromURI)
				.sequenceValidated
				.flatMap			{ _.toNesOption toGood badMessage(s"empty uri list") }
		
		private def fileFromURI(uri:String):Validated[Exception,File]	=
				 Catch.exception in new File(new URI(uri)) mapFail Nes.single into Validated.fromTried
		
		private def filesFromJList(jlist:JList[File]):Validated[Exception,Nes[File]]	=
				jlist.toISeq.toNesOption toGood badMessage(s"empty file list")
								
		private def filesFromURL(url:URL):Validated[Exception,Nes[File]]	=
				url.toFile toGood badMessage(s"not a file url: ${url}") map Nes.single 
			
		//------------------------------------------------------------------------------
			
		private def badMessage(message:String):Nes[Exception]	=
				Nes single new IOException(message)
	}
}
