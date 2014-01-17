\name{add_key}
\alias{add_key}
\title{Add a key to connect observations}
\usage{
add_key(obs, parent, recycle, key.name = "key_name", quiet = FALSE)
}
\arguments{
  \item{obs}{list. Should be the output from
  \link{listsToObs}.}

  \item{parent}{character string. Should be present in the
  names of \code{obs}.}

  \item{recycle}{character string that matches a variable
  name among \code{parent} observations. Note that if this
  argument is used, \code{key.name} is ignored.}

  \item{key.name}{The desired column name of the newly
  generated key.}

  \item{quiet}{logical. Include message about the keys
  being generated?}
}
\value{
A list of observations.
}
\description{
This function takes a list of "observations" (that is, a
list of matrices with one row) and appends a new column to
each relevant observation. This column is a 'key' that
connects a specified \code{parent} to each one of its
descendants. This 'key' is meant to be used for
merging/joining tables at a later point (that is, after
observations have been \link{collapse}d into tables).
}

