package scutil.security

import java.security.*
import java.text.Normalizer
import java.nio.charset.Charset

import scutil.core.implicits.*
import scutil.lang.*
import scutil.codec.Base64

/*
object HashSalt {
	def main(args:Array[String]) {
		import default.*

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
// TODO scala get this back
//@throws(classOf[NoSuchAlgorithmException])
final class HashSalt(
	encoding:Charset,
	normalizerForm:Normalizer.Form,
	hashAlgorithm:String,
	randomAlgorithm:String,
	saltSize:Int,
	roundCount:Int
) {
	private val	random	= SecureRandom getInstance randomAlgorithm

	/** salt and cook a raw password so it can safely be stored somewhere */
	@throws(classOf[NoSuchAlgorithmException])
	def cook(raw:String):String	= {
		val	salt		= synchronized { random byteString saltSize }
		val prepared	= prepare(raw, salt, roundCount)
		roundCount.toString				+ "$" +
		(Base64 encodeByteString salt)	+ "$" +
		(Base64 encodeByteString prepared)
	}

	/** check if a raw password, when cooked, matches the same password cooked before */
	@throws(classOf[NoSuchAlgorithmException])
	def taste(raw:String, cooked:String):Boolean = {
		(for {
			Seq(r,s,p)	<- cooked splitAroundChar '$' optionBy { _.size == 3 }
			rounds		<- r.toIntOption
			salt		<- Base64 decodeByteString s
			prepared	<- Base64 decodeByteString p
		}
		yield {
			prepared constantTimeEquals prepare(raw, salt, rounds)
		})
		.getOrElse(false)
	}

	@throws(classOf[NoSuchAlgorithmException])
	private def prepare(raw:String, salt:ByteString, rounds:Int):ByteString	=
		raw											|>
		(Normalizer.normalize(_, normalizerForm))	|>
		(_ toByteString encoding)					|>
		(salt ++ _)									|>
		hash(rounds)

	@throws(classOf[NoSuchAlgorithmException])
	private def hash(rounds:Int)(bytes:ByteString):ByteString	=
		Hashing.hash(hashAlgorithm, rounds, bytes)
}
