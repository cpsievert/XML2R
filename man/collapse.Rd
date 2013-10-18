\name{collapse}
\alias{collapse}
\title{Collapse a list of observations into a list of tables.}
\usage{
  collapse(obs)
}
\arguments{
  \item{obs}{list of observations.}
}
\value{
  Returns list with one element for each relevant XML node.
  Each element contains a matrix.
}
\description{
  This function aggregates all observations with a similar
  name into a common table. Note that observations with a
  particular name don't need consistent variables (any
  missing information is filled with NAs).
}

