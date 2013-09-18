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
m <- listsToMatrix(l)
names(m)
m[[1]]
m[[2]]
df <- lapply(m, function(x) data.frame(x, stringsAsFactors=FALSE))

m2 <- listsToMatrix(l2)
names(m2)
m2[[1]]
}

