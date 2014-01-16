\name{re_name}
\alias{re_name}
\title{Rename rows of a list}
\usage{
re_name(obs, namez, equiv, diff.name = "diff_name", rename.as,
  quiet = FALSE)
}
\arguments{
  \item{obs}{list. Should be the output from \link{XML2Obs}
  (or \link{listsToObs}).}

  \item{namez}{must be equivalent to \code{names(obs)}.
  Intended use is to avoid unneccessarily repeating that
  operation.}

  \item{equiv}{character vector with the appropriate
  (unique) names that should be regarded "equivalent".}

  \item{diff.name}{character string used for naming the
  variable that is appended to any observations whose name
  was overwritten. The value for this variable is the
  difference in from the original name and the overwritten
  name.}

  \item{rename.as}{character string to override naming of
  observations that are renamed.}

  \item{quiet}{logical. Include message about how
  observations are being renamed?}
}
\value{
A list of "observations".
}
\description{
Sometimes, certain nodes in an XML ancestory may want to be
neglected before any keys are created (see \link{add_key})
or observations are aggregated (see \link{collapse}). This
function takes a list of "observations" (that is, a list of
matrices with one row) and alters the names of that list.
Note that any information lost from changing names is saved
in a new column whose name is specified by
\code{diff.name}.
}

