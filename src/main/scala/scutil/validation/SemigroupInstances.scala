package scutil.validation

object SemigroupInstances {
	implicit val NonEmptyList_Semigroup	= new Semigroup[NonEmptyList[String]] {
		def append(a:NonEmptyList[String], b:NonEmptyList[String]):NonEmptyList[String]	= a ++ b
	} 
}
