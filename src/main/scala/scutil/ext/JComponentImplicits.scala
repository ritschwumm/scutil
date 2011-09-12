package scutil.ext

import java.awt.{ List=>AwtList, _ }
import java.awt.event._
import javax.swing._
import javax.swing.event._

import RectangleImplicits._
import RootPaneContainerImplicits._

object JComponentImplicits extends JComponentImplicits

trait JComponentImplicits {
	implicit def toJComponentExt[T <: JComponent](delegate:T):JComponentExt[T] = new JComponentExt[T](delegate)
}
	
final class JComponentExt[T <: JComponent](delegate:T) {
	def innerRectangle:Rectangle	= 
			new Rectangle(delegate.getSize()) inset delegate.getInsets
			
	def displayInFrame(size:(Int,Int), onClose:()=>Boolean=()=>true):JFrame = {
		val	frame	= new JFrame
		frame setCenterContent	delegate
		frame setVisible		true
		frame setSize			(size._1, size._2)
		/*
		frame setUndecorated true
		frame setSize Toolkit.getDefaultToolkit.getScreenSize
		frame setAlwaysOnTop true
		frame setLocation new Point
		*/
		frame setDefaultCloseOperation WindowConstants.DO_NOTHING_ON_CLOSE
		
		def doClose() {
			if (onClose()) frame.dispose()
		}
		
		frame addWindowListener new WindowAdapter {
			override def windowClosing(ev:WindowEvent) {
				doClose()
			}
		}
		frame.getRootPane registerKeyboardAction (
			new ActionListener {
				def actionPerformed(ev:ActionEvent) {
					doClose()
				}
			},
			KeyStroke getKeyStroke (KeyEvent.VK_ESCAPE, 0),
			JComponent.WHEN_IN_FOCUSED_WINDOW
		)
		
		frame
	}
}
