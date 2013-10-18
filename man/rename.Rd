\name{rename}
\alias{rename}
\title{Rename rows of a list}
\usage{
  rename(obs, equiv, diff.name = "diff_name")
}
\arguments{
  \item{obs}{list. Should be the output from
  \link{listsToObs}.}

  \item{equiv}{character vector with the appropriate
  (unique) names from \code{r} that should be regarded
  "equivalent".}

  \item{diff.name}{character string used for naming the
  variable that is appended to any observations whose name
  was overwritten. The value for this variable is the
  difference in from the original name and the overwritten
  name.}
}
\value{
  A list of "observations".
}
\description{
  This function takes a list of "observations" (that is, a
  list of matrices with one row) and changes the names of
  that list. The names are intended to reflect the XML node
  ancestory for which the observation was extracted from.
  Sometimes, certain nodes in an XML ancestory may want to
  be neglected before any keys are created (see
  \link{add_key}) or observations are aggregated (see
  \link{collapse}). Note that neglected nodes are saved in
  the "neglected" attribute of any observation whose name
  was overwritten.
}

