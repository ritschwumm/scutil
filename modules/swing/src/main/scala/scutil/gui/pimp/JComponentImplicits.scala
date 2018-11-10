package scutil.gui.pimp

import java.awt.{ List=>_, _ }
import java.awt.event._
import javax.swing._

import scutil.lang._
import scutil.gui.pimp.RootPaneContainerImplicits._

object JComponentImplicits extends JComponentImplicits

trait JComponentImplicits {
	implicit final class JComponentExt(peer:JComponent) {
		def innerRectangle:Rectangle	= {
			val insets	= peer.getInsets
			new Rectangle(
				0 + insets.left,
				0 + insets.top,
				peer.getWidth	- insets.left	- insets.right,
				peer.getHeight	- insets.top	- insets.bottom
			)
		}

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
}
