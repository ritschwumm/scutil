package scutil

import scutil.ext._

object Implicits extends Implicits
trait Implicits 
		extends	LangImplicits 
		with	IoImplicits
		with	GuiImplicits
		
object LangImplicits extends LangImplicits
trait LangImplicits
		extends	AnyImplicits 
		with	AnyRefImplicits
		with	BooleanImplicits
		with	DateImplicits
		with	EitherImplicits
		with	Function1Implicits
		with	IterableImplicits
		with	ListImplicits
		with	OptionImplicits
		with	PairImplicits
		with	SeqImplicits
		with	SetImplicits
		with	StringImplicits
		with	TraversableImplicits
		
object IoImplicits extends IoImplicits		
trait IoImplicits
		extends	FileImplicits
		with	InputStreamImplicits
		with	ReaderImplicits
		with	WriterImplicits
		with	URLImplicits
		
object GuiImplicits extends GuiImplicits				
trait GuiImplicits
		extends	ComponentImplicits
		with	JComponentImplicits
		with	RectangleImplicits
