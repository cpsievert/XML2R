\name{add_key}
\alias{add_key}
\title{Add a key to connect parents to descendants}
\usage{
add_key(obs, parent, recycle, key.name, quiet = FALSE)
}
\arguments{
  \item{obs}{list. Should be the output from
  \link{listsToObs}.}

  \item{parent}{character string. Should be present in the
  names of \code{obs}.}

  \item{recycle}{character string that matches a variable
  name among \code{parent} observations.}

  \item{key.name}{The desired column name of the newly
  generated key.}

  \item{quiet}{logical. Include message about the keys
  being generated?}
}
\value{
A list of observations.
}
\description{
This function creates a mapping from parent observations to
it's descendants (which useful for merging/joining tables).
Either an existing value in the parent observation can be
\code{recycle}d to it's descendants or a new column will be
created (if \code{recycle} is missing).
}

