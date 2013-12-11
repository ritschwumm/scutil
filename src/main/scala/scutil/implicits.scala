package scutil

import scutil.pimp._

object implicits extends implicits
trait implicits 
		extends	langImplicits
		with	javaImplicits
		with	ioImplicits
		with	guiImplicits
		with	DisposableConversions
		
object langImplicits extends langImplicits
trait langImplicits
		extends	AnyImplicits 
		with	AnyRefImplicits
		with	BooleanImplicits
		with	ExceptionCatchImplicits
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
		
object javaImplicits extends javaImplicits
trait javaImplicits
		extends	ClassImplicits
		with	DateImplicits
		with	DateFormatImplicits
		with	MatcherImplicits
		with	StringImplicits
		with	StringTokenizerImplicits
		with	ThrowableImplicits
		
object ioImplicits extends ioImplicits
trait ioImplicits
		extends	FileImplicits
		with	InputStreamImplicits
		with	OutputStreamImplicits
		with	ReaderImplicits
		with	URLImplicits
		
object guiImplicits extends guiImplicits
trait guiImplicits
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
