package scutil.search

case class SearchPattern(positive:Seq[SearchToken], negative:Seq[SearchToken])
