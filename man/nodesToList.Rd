\name{nodesToList}
\alias{nodesToList}
\title{Coerce XML Nodes into a list with both attributes and values}
\usage{
  nodesToList(nodes)
}
\description{
  Essentially a recursive call to \link{xmlToList}.
}
\examples{
l <- nodesToList(t)
#finds the number of levels in the list -- which should correspond to the number of xmlChildren
depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
depth(l)
l2 <- nodesToList(t2)
depth(l2)
}

