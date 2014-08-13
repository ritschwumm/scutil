package scutil.search

import scutil.lang.ISeq

final case class SearchPattern(positive:ISeq[SearchToken], negative:ISeq[SearchToken])
