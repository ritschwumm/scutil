package scutil.time

final case class Year(value:Int) {
	def gregorianLeap:Boolean	=
			(value % 4 == 0) 	&&
			!(value % 100 == 0) ||
			(value % 400 == 0) 
}
