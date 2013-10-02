package scutil.search

final case class SearchPattern(positive:Seq[SearchToken], negative:Seq[SearchToken])
