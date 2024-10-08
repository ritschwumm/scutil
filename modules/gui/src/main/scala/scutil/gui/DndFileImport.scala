package scutil.gui

import java.io.{ File, IOException, FileNotFoundException }
import java.util.{ List as JList }
import java.net.{ URI, URL }
import java.awt.datatransfer.DataFlavor
import javax.swing.{ JComponent, TransferHandler }
import javax.swing.TransferHandler.TransferSupport

import scutil.lang.*
import scutil.core.implicits.*
import scutil.jdk.implicits.*
import scutil.gui.implicits.*
import scutil.geom.IntPoint

object DndFileImport {
	// TODO using this is a Using
	def install(target:JComponent, consumer:IntPoint=>Option[Effect[Validated[Nes[Exception],Nes[File]]]]):Disposer	= {
		target.setTransferHandler(new FileTransferHandler(consumer))

		Disposer delay {
			target.setTransferHandler(null)
		}
	}

	private val importable:Set[DataFlavor]	=
		Set(
			DndFlavors.javaFileList,
			DndFlavors.uriList,
			DndFlavors.url
		)

	private final class FileTransferHandler(consumer:IntPoint=>Option[Effect[Validated[Nes[Exception],Nes[File]]]]) extends TransferHandler {
		override def canImport(support:TransferSupport):Boolean =
			importEffect(support).isDefined

		override def importData(support:TransferSupport):Boolean =
			importEffect(support).cata(
				// wrong format or bad place
				false,
				effect => {
					//@see http://www.davidgrant.ca/drag_drop_from_linux_kde_gnome_file_managers_konqueror_nautilus_to_java_applications
					val extracted:Validated[Nes[Exception],Nes[File]]	=
						// linux
						extractFileList[String]		(support, DndFlavors.uriList,		filesFromURIList)	orElse
						// windows / osx
						extractFileList[JList[File]](support, DndFlavors.javaFileList,	filesFromJList)		orElse
						// not actually used?
						extractFileList[URL]		(support, DndFlavors.url,			filesFromURL)		getOrElse
						// unknown format
						Validated.invalid(invalidMessage("unexpected transfer flavor"))
					extracted.doto(effect).isValid
				}
			)

		//------------------------------------------------------------------------------

		private def importEffect(support:TransferSupport):Option[Effect[Validated[Nes[Exception],Nes[File]]]] =
			supportsFormat(support).flatOption(consumer(dropIntPoint(support)))

		private def supportsFormat(support:TransferSupport):Boolean =
			support.getDataFlavors.toSet.containsAny(DndFileImport.importable)

		private def dropIntPoint(support:TransferSupport):IntPoint	=
			support.getDropLocation.getDropPoint.toIntPoint

		private def extractFileList[T](support:TransferSupport, flavor:DataFlavor, extractor:T=>Validated[Nes[Exception],Nes[File]]):Option[Validated[Nes[Exception],Nes[File]]]	=
			support.isDataFlavorSupported(flavor).option {
				extractTransferData[T](support, flavor).leftMap(Nes.one).toValidated.flatMap(extractor)
			}

		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		private def extractTransferData[T](support:TransferSupport, flavor:DataFlavor):Either[Exception,T]	=
			Catch.exception.in(
				support.getTransferable.getTransferData(flavor).asInstanceOf[T]
			)

		private def filesFromURIList(uriList:String):Validated[Nes[Exception],Nes[File]]	=
			uriList
			.split				("\r|\n")
			.toVector
			.filterNot			{ _.isEmpty			}
			.filterNot			{ _.startsWith("#")	}
			.traverseValidated	(fileFromURI)
			.flatMap			{ _.toNesOption.toValid(invalidMessage(s"empty uri list")) }

		// on el captain text/uri-list contains plain file path, but new File(URI) expects an absolute URI
		private def fileFromURI(s:String):Validated[Nes[Exception],File]	=
			fileFromURI1(s).leftMap(Nes.one).toValidated	`or`
			(fileFromURI2(s).leftMap(Nes.one).toValidated)

		private def fileFromURI1(s:String):Either[Exception,File]	=
			Catch.exception in {
				val uri	= new URI(s)
				if (uri.getScheme != null)	new File(uri)
				else						new File(s)
			}

		private def fileFromURI2(s:String):Either[Exception,File]	=
			Catch.exception in {
				val file	= new File(s)
				if (!file.exists)	throw new FileNotFoundException(s"file does not exist: ${file.toString}")
				file
			}

		private def filesFromJList(jlist:JList[File]):Validated[Nes[Exception],Nes[File]]	=
			jlist.toSeq.toNesOption.toValid(invalidMessage(s"empty file list"))

		private def filesFromURL(url:URL):Validated[Nes[Exception],Nes[File]]	=
			url.toFile.toValid(invalidMessage(s"not a file url: ${url.toString}")).map(Nes.one)

		//------------------------------------------------------------------------------

		private def invalidMessage(message:String):Nes[Exception]	=
			Nes.one(new IOException(message))
	}
}
