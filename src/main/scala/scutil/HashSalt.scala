package scutil

import java.security._
import java.text.Normalizer

import scutil.LangImplicits._

/*
object HashSalt {
	def main(args:Array[String]) {
		import default._
		
		args match {
			case Array(password)	=>
				val cooked	= cook(password)
				System.out println cooked
			case Array(password, cooked)	=>
				val ok	= taste(password, cooked)
				if (ok)	System.out println	"success"
				else	System.out println	"failure"
			case _ =>
				System.err println "usage: [ password | password cooked ]"
				sys exit 2
		}
	}
	
	lazy val default	= new HashSalt(
		encoding			= "utf-8",
		normalizerForm		= Normalizer.Form.NFC,
		hashAlgorithm		= "SHA-512",
		randomAlgorithm		= "SHA1PRNG",
		saltSize			= 16,
		roundCount			= 2048
	)
}
*/

/** 
hashed and salted passwords:
hashing hides the cleartext,
salting makes dictionary attacks more expensive,
multiple rounds help against brute force attacks.
*/
final class HashSalt(
	encoding:String,
	normalizerForm:Normalizer.Form,
	hashAlgorithm:String,
	randomAlgorithm:String,
	saltSize:Int,
	roundCount:Int
) {
	private val	random	= SecureRandom getInstance randomAlgorithm
	
	/** salt and cook a raw password so it can safely be stored somewhere */
	def cook(raw:String):String	= {
		val	salt		= new Array[Byte](saltSize) |>> synchronized { random.nextBytes }
		val prepared	= prepare(raw, salt, roundCount)
		roundCount				+ "$" + 
		(salt |> Base64.apply)	+ "$" +
		(prepared |> Base64.apply)
	}
		
	/** check if a raw password, when cooked, matches the same password cooked before */
	def taste(raw:String, cooked:String):Boolean = {
		(for {
			Seq(r,s,p)	<- cooked splitAround '$' guardBy { _.size == 3 }
			rounds		<- r.toIntOption
			salt		<- Base64 unapply s
			prepared	<- Base64 unapply p
		}
		yield {
			prepared sameElements prepare(raw, salt, rounds)
		})
		.getOrElse(false)
	}
	
	private def prepare(raw:String, salt:Array[Byte], rounds:Int):Array[Byte]	=
			raw 										|> 
			(Normalizer normalize (_, normalizerForm)) 	|>
			(_ getBytes encoding) 						|> 
			(salt ++ _) 								|> 
			hash(rounds)
	
	private def hash(rounds:Int)(bytes:Array[Byte]):Array[Byte]	= {
		// throws NoSuchAlgorithmException
		val	digest	= MessageDigest getInstance hashAlgorithm
		var trip	= bytes
		var round	= 0
		while (round < rounds) {
			digest update trip
			trip	= digest.digest()
			round	+= 1
		}
		trip
	}
}