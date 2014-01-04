\name{listsToObs}
\alias{listsToObs}
\title{Flatten nested list into a list of observations}
\usage{
  listsToObs(l, append.value = TRUE, as.equiv = TRUE)
}
\arguments{
  \item{l}{list. Should be the output from
  \link{nodesToList}.}

  \item{append.value}{logical. Should the XML value be
  appended to the observation?}
}
\value{
  A list where each element reflects one "observation".
}
\description{
  This function flattens the nested list into a list of
  "observations" (that is, a list of matrices with one
  row). The names of the list that is returned reflects the
  XML ancestory of each observation.
}

