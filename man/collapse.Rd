\name{collapse}
\alias{collapse}
\title{Collapse a list of matrices (or data frames) into a smaller list of larger matrices or data frames}
\usage{
  collapse(m, condition = "url")
}
\arguments{
  \item{m}{list of matrices or data frames}

  \item{condition}{A partial match of names(m).}
}
\value{
  Returns list with one element for each relevant XML node.
  Each element contains a matrix by default.
}
\description{
  Collapse a list of matrices (or data frames) into a
  smaller list of larger matrices or data frames
}

