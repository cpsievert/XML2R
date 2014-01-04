\name{add_key}
\alias{add_key}
\title{Add a key to connect observations}
\usage{
  add_key(obs, parent, child, key.name = "key_name",
    quiet = FALSE)
}
\arguments{
  \item{obs}{list. Should be the output from
  \link{listsToObs}.}

  \item{parent}{character string. Should be present in the
  names of \code{obs}.}

  \item{child}{character string. Should be present in the
  names of \code{obs}.}

  \item{key.name}{The desired column name of the newly
  generated key.}

  \item{quiet}{logical. Include message about the keys
  being generated?}
}
\value{
  A list of "observations".
}
\description{
  This function takes a list of "observations" (that is, a
  list of matrices with one row) and appends a new column
  to each relevant observation. This column is a key that
  connects \code{parent}s to \code{child}ren in case their
  observations need to be joined together at a later point.
}

