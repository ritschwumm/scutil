package scutil

import scutil.pimp._

object Implicits extends Implicits
trait Implicits 
		extends	LangImplicits
		with	JavaImplicits
		with	IoImplicits
		with	GuiImplicits
		with	DisposableConversions
		
object LangImplicits extends LangImplicits
trait LangImplicits
		extends	AnyImplicits 
		with	AnyRefImplicits
		with	BooleanImplicits
		with	CatchImplicits
		with	EitherImplicits
		with	ElemImplicits
		with	ExecutorImplicits
		with	Function0Implicits
		with	Function1Implicits
		with	Function2Implicits
		with	HomogenousPairImplicits
		with	IteratorImplicits
		with	ListImplicits
		with	MapImplicits
		with	NodeImplicits
		with	OptionImplicits
		with	OrderingImplicits
		with	PFunctionImplicits
		with	PairImplicits
		with	PartialFunctionImplicits
		with	PredicateImplicits
		with	QueueImplicits
		with	RegexImplicits
		with	SeqImplicits
		with	SetImplicits
		with	TraversableImplicits
		with	TraversableOnceImplicits
		with	TryImplicits
		
object JavaImplicits extends JavaImplicits
trait JavaImplicits
		extends	ClassImplicits
		with	DateImplicits
		with	DateFormatImplicits
		with	MatcherImplicits
		with	StringImplicits
		with	StringTokenizerImplicits
		with	ThrowableImplicits
		
object IoImplicits extends IoImplicits		
trait IoImplicits
		extends	FileImplicits
		with	InputStreamImplicits
		with	ReaderImplicits
		with	URLImplicits
		
object GuiImplicits extends GuiImplicits				
trait GuiImplicits
		extends	ColorImplicits
		with	ComponentImplicits
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
		with	WindowImplicits
