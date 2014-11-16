package scutil.gui.pimp

import java.awt.{ List=>AwtList, _ }
import java.awt.event._
import javax.swing._

import scutil.lang._
import scutil.gui.pimp.RectangleImplicits._
import scutil.gui.pimp.RootPaneContainerImplicits._

object JComponentImplicits extends JComponentImplicits

trait JComponentImplicits {
	implicit def toJComponentExt(peer:JComponent):JComponentExt	= new JComponentExt(peer)
}
	
final class JComponentExt(peer:JComponent) {
	def innerRectangle:Rectangle	= 
			new Rectangle(peer.getSize()) inset peer.getInsets
			
	def displayInFrame(size:Dimension, onClose:Thunk[Boolean] = thunk{true}):JFrame = {
		val	frame	= new JFrame
		frame setCenterContent	peer
		frame setVisible		true
		frame setSize			size
		frame setDefaultCloseOperation WindowConstants.DO_NOTHING_ON_CLOSE
		
		def doClose() {
			if (onClose()) {
				frame setVisible false
				frame.dispose()
			}
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
