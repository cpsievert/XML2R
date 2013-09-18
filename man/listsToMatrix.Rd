\name{listsToMatrix}
\alias{listsToMatrix}
\title{Coerce lists of character vectors into lists of matrices}
\usage{
  listsToMatrix(l)
}
\value{
  A list of matrices. Each element name reflects where in
  the list hierarchy the information came from.
}
\description{
  This function gathers vectors under the same level of the
  list hierarchy and binds those common vectors into a
  matrix. In XML terms, this function coerces XML
  attributes and XML values into a matrix for a particular
  XML node.
}
\examples{
l <- nodesToList(t)
#finds the number of levels in the list -- which should correspond to the number of xmlChildren
depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
depth(l)
l2 <- nodesToList(t2)
depth(l2)
}

