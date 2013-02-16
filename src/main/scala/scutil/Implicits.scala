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
		with	CatchImplicits
		with	ClassImplicits
		with	DateImplicits
		with	DateFormatImplicits
		with	EitherImplicits
		with	ElemImplicits
		with	ExecutorImplicits
		with	Function0Implicits
		with	Function1Implicits
		with	Function2Implicits
		with	IteratorImplicits
		with	ListImplicits
		with	MapImplicits
		with	MatcherImplicits
		with	NodeImplicits
		with	OptionImplicits
		with	OrderingImplicits
		with	PairImplicits
		with	PartialFunctionImplicits
		with	PredicateImplicits
		with	QueueImplicits
		with	RegexImplicits
		with	SeqImplicits
		with	SetImplicits
		with	StringImplicits
		with	StringTokenizerImplicits
		with	ThrowableImplicits
		with	TraversableImplicits
		with	TraversableOnceImplicits
		with	TryImplicits
		
object IoImplicits extends IoImplicits		
trait IoImplicits
		extends	FileImplicits
		with	InputStreamImplicits
		with	ReaderImplicits
		with	URLImplicits
		
object GuiImplicits extends GuiImplicits				
trait GuiImplicits
		extends	ComponentImplicits
		with	ContainerImplicits
		with	DimensionImplicits
		with	GraphicsImplicits
		with	ImageImplicits
		with	ImageIconImplicits
		with	InsetsImplicits
		with	JComponentImplicits
		with	PointImplicits
		with	RectangleImplicits
		with	RootPaneContainerImplicits
