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
		with	ClassImplicits
		with	DateImplicits
		with	DateFormatImplicits
		with	EitherImplicits
		with	Function1Implicits
		with	ListImplicits
		with	NodeImplicits
		with	OptionImplicits
		with	PairImplicits
		with	PredicateImplicits
		with	RegexImplicits
		with	SeqImplicits
		with	SetImplicits
		with	StringImplicits
		with	TraversableImplicits
		with	TraversableOnceImplicits
		
object IoImplicits extends IoImplicits		
trait IoImplicits
		extends	FileImplicits
		with	InputStreamImplicits
		with	ReaderImplicits
		with	URLImplicits
		
object GuiImplicits extends GuiImplicits				
trait GuiImplicits
		extends	ComponentImplicits
		with	JComponentImplicits
		with	RectangleImplicits
		with	RootPaneContainerImplicits
