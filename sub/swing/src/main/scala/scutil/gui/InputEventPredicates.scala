package scutil.gui

import java.awt.event._

import scutil.lang._

object InputEventPredicates {
	//------------------------------------------------------------------------------
	//## actions
	
	val mousePressed:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_PRESSED)
	val mouseReleased:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_RELEASED)
	val mouseClicked:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_CLICKED)
	
	val mouseDragged:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_DRAGGED)
	val mouseMoved:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_MOVED)
	val mouseEntered:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_ENTERED)
	val mouseExited:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_EXITED)
	
	val mouseWheeled:Predicate[MouseEvent]	= eventID(MouseEvent.MOUSE_WHEEL)
	
	val keyPressed:Predicate[KeyEvent]		= eventID(KeyEvent.KEY_PRESSED)
	val keyTyped:Predicate[KeyEvent]		= eventID(KeyEvent.KEY_TYPED)
	val keyReleased:Predicate[KeyEvent]		= eventID(KeyEvent.KEY_RELEASED)
	
	//------------------------------------------------------------------------------
	//## buttons
	
	val button1:Predicate[MouseEvent]		= button(MouseEvent.BUTTON1)
	val button2:Predicate[MouseEvent]		= button(MouseEvent.BUTTON2)
	val button3:Predicate[MouseEvent]		= button(MouseEvent.BUTTON3)
	
	//------------------------------------------------------------------------------
	//## modifiers
	
	val buttonDown1:Predicate[InputEvent]	= modifierSet(InputEvent.BUTTON1_DOWN_MASK)
	val buttonDown2:Predicate[InputEvent]	= modifierSet(InputEvent.BUTTON2_DOWN_MASK)
	val buttonDown3:Predicate[InputEvent]	= modifierSet(InputEvent.BUTTON3_DOWN_MASK)
	
	val buttonUp1:Predicate[InputEvent]		= modifierClear(InputEvent.BUTTON1_DOWN_MASK)
	val buttonUp2:Predicate[InputEvent]		= modifierClear(InputEvent.BUTTON2_DOWN_MASK)
	val buttonUp3:Predicate[InputEvent]		= modifierClear(InputEvent.BUTTON3_DOWN_MASK)
	 	
	val shiftDown:Predicate[InputEvent]		= modifierSet(InputEvent.SHIFT_DOWN_MASK)
	val altGraphDown:Predicate[InputEvent]	= modifierSet(InputEvent.ALT_GRAPH_DOWN_MASK)
	val altDown:Predicate[InputEvent]		= modifierSet(InputEvent.ALT_DOWN_MASK)
	val ctrlDown:Predicate[InputEvent]		= modifierSet(InputEvent.CTRL_DOWN_MASK)
	val metaDown:Predicate[InputEvent]		= modifierSet(InputEvent.META_DOWN_MASK)
	
	val shiftUp:Predicate[InputEvent]		= modifierClear(InputEvent.SHIFT_DOWN_MASK)
	val altGraphUp:Predicate[InputEvent]	= modifierClear(InputEvent.ALT_GRAPH_DOWN_MASK)
	val altUp:Predicate[InputEvent]			= modifierClear(InputEvent.ALT_DOWN_MASK)
	val ctrlUp:Predicate[InputEvent]		= modifierClear(InputEvent.CTRL_DOWN_MASK)
	val metaUp:Predicate[InputEvent]		= modifierClear(InputEvent.META_DOWN_MASK)
	
	//------------------------------------------------------------------------------
	//## utils
	
	def eventID(id:Int):Predicate[InputEvent]			= _.getID == id
	
	def button(button:Int):Predicate[MouseEvent]		= ev => ev.getButton == button
	
	def modifierSet(mask:Int):Predicate[InputEvent]		= ev => (ev.getModifiersEx & mask) == mask
	def modifierClear(mask:Int):Predicate[InputEvent]	= ev => (ev.getModifiersEx & mask) == 0
}
