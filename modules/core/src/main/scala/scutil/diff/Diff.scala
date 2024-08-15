package scutil.diff

import scala.collection.mutable

/**
based on Scala	code by nafg		at https://github.com/nafg/reactive/blob/master/reactive-core/src/main/scala/reactive/LCS.scala
based on Java	code by Neil Jones	at http://bix.ucsd.edu/bioalgorithms/downloads/code/LCS.java
*/
object Diff {
	private def equalsMethod[T](a:T, b:T):Boolean	= a == b

	// BETTER only compare from the first different element to the last different element
	def compile[T](a:Seq[T], b:Seq[T], equal:(T,T)=>Boolean = equalsMethod[T](_,_)):Diff[T] = {
		val n	= a.length
		val m	= b.length

		val S	= Array.ofDim[Int]			(n+1, m+1)
		val R	= Array.ofDim[DiffDirection](n+1, m+1)

		// It is important to use to, not until.
		// The next two for-loops are initialization
		for (ii <- 0 to n) {
			S(ii)(0) = 0
			R(ii)(0) = DiffDirection.Up
		}
		for (jj <- 0 to m) {
			S(0)(jj) = 0
			R(0)(jj) = DiffDirection.Left
		}

		// This is the main dynamic programming loop that
		// computes the score and backtracking arrays.
		for {
			ii <- 1 to n
			jj <- 1 to m
		} {
			if (equal(a(ii-1), b(jj-1))) {
				S(ii)(jj)	= S(ii-1)(jj-1) + 1
				R(ii)(jj)	= DiffDirection.UpAndLeft
			}
			else {
				S(ii)(jj)	= S(ii-1)(jj-1) + 0
				R(ii)(jj)	= DiffDirection.Neither
			}

			if (S(ii-1)(jj) >= S(ii)(jj)) {
				S(ii)(jj)	= S(ii-1)(jj)
				R(ii)(jj)	= DiffDirection.Up
			}

			if (S(ii)(jj-1) >= S(ii)(jj)) {
				S(ii)(jj)	= S(ii)(jj-1)
				R(ii)(jj)	= DiffDirection.Left
			}
		}

		// The length of the longest substring is S[n][m]
		//val pos		= S(n)(m) - 1

		// Trace the backtracking matrix.
		var diffs	= Vector.empty[Delta[T]]
		var ii	= n
		var jj	= m
		while (ii > 0 || jj > 0) {
			R(ii)(jj) match {
				case DiffDirection.UpAndLeft =>
					ii		-= 1
					jj		-= 1
				case DiffDirection.Up =>
					ii		-= 1
					diffs	+:= Delta.Remove(ii, a(ii))
				case DiffDirection.Left =>
					jj		-= 1
					diffs	+:= Delta.Include(jj, b(jj))
				case DiffDirection.Neither =>
					// nothing to do here
			}
		}

		var offset	= 0
		val deltas	= diffs.map {
			case Delta.Include(index, element) =>
				offset	+= 1
				Delta.Include(index, element):Delta[T]
			case Delta.Remove(index, element) =>
				offset	-= 1
				Delta.Remove(index + offset + 1, element):Delta[T]
		}

		Diff(deltas)
	}
}

final case class Diff[T](deltas:Seq[Delta[T]]) {
	def patch(iseq:Seq[T]):Seq[T]	= {
		val out	= mutable.ArrayBuffer(iseq*)
		deltas foreach {
			case Delta.Include(index, element)	=> out.insert(index, element)
			case Delta.Remove(index, element)	=> out.remove(index)
		}
		out.toVector
	}
}
