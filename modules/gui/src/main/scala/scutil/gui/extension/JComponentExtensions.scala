package scutil.gui.extension

import java.awt.{ List as _, * }
import java.awt.event.*
import javax.swing.*

import scutil.lang.*
import scutil.gui.extension.RootPaneContainerExtensions.*

object JComponentExtensions {
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

			def doClose():Unit	= {
				if (onClose()) {
					frame setVisible false
					frame.dispose()
				}
			}

			frame addWindowListener new WindowAdapter {
				override def windowClosing(ev:WindowEvent):Unit	= {
					doClose()
				}
			}
			frame.getRootPane.registerKeyboardAction(
				new ActionListener {
					def actionPerformed(ev:ActionEvent):Unit	= {
						doClose()
					}
				},
				KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
				JComponent.WHEN_IN_FOCUSED_WINDOW
			)

			frame
		}
	}
}
