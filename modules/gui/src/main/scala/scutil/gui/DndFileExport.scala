package scutil.gui

import java.io.*
import java.awt.dnd.*
import java.awt.datatransfer.*
import javax.swing.*

import scutil.lang.*
import scutil.jdk.implicits.*
import scutil.platform.*
import scutil.geom.*
import scutil.gui.implicits.*

object DndFileExport {
	// TODO using this is a Using
	def install(target:JComponent, provider:IntPoint=>Option[Nes[File]]):Disposer	= {
		val dragGestureListener	=
			new DragGestureListener {
				def dragGestureRecognized(ev:DragGestureEvent):Unit	= {
					provider(ev.getDragOrigin.toIntPoint)
					.map 		{ new FileTransferable(_) }
					.foreach	{ it =>
						ev.startDrag(DragSource.DefaultCopyDrop, it)
					}
				}
			}

		val dragSource	= DragSource.getDefaultDragSource

		// NOTE allowing ACTION_COPY_OR_MOVE (at least on linux) leads to a MOVE with nautilus
		val dragGestureRecognizer	=
			dragSource.createDefaultDragGestureRecognizer(
				target,
				DnDConstants.ACTION_COPY,
				dragGestureListener
			)

		Disposer delay {
			dragGestureRecognizer setComponent null
		}
	}

	private val exportable:Seq[DataFlavor]	=
		OperatingSystem.current match {
			case Some(OperatingSystem.OSX)		=>
				Seq(DndFlavors.javaFileList)
			case Some(OperatingSystem.Windows)	=>
				Seq(DndFlavors.javaFileList)
			case Some(OperatingSystem.Linux)	=>
				Seq(
					DndFlavors.javaFileList,
					DndFlavors.uriList,
					DndFlavors.url
					// DndFlavors.binaryFlavor
				)
			case None	=>
				Seq.empty
		}

	private final class FileTransferable(files:Nes[File]) extends Transferable {
		def isDataFlavorSupported(flavor:DataFlavor):Boolean	=
			getTransferDataFlavors contains flavor

		def getTransferDataFlavors:Array[DataFlavor] =
			exportable.toArray

		def getTransferData(flavor:DataFlavor):AnyRef	=
			flavor match {
				case DndFlavors.javaFileList	=>
					// NOTE this is the only one that actually can transfer multiple files
					files.toVector.toJList
				case DndFlavors.uriList		=>
					// NOTE this results in file:/tmp/path instead of file:///tmp/path
					files.head.toURI.toASCIIString + "\r\n"
				case DndFlavors.url		=>
					// NOTE does not work on windows
					files.head.toURI.toURL
				case DndFlavors.binary	=>
					// NOTE doesn't give a file name
					// NOTE does not work on windows
					files.head.newInputStream()
				case x	=>
					sys error s"unexpected DataFlavor ${x.toString}"
			}
	}
}
