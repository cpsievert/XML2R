\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into data frame(s)}
\usage{
  urlsToDataFrame(urls, tables = list(),
    extract = "attributes")
}
\arguments{
  \item{urls}{set of urls for parsing}

  \item{tables}{list of character vectors with appropriate
  names. The list names should correspond to XML nodes of
  interest within the XML files.}

  \item{extract}{for each element of \code{tables},
  \code{extract} can be used to specify whether
  "attributes", "values" or "both" should be extracted.}
}
\value{
  Returns a data frames if the length of tables is one.
  Otherwise, it returns a list of data frames.
}
\description{
  This function takes on a list of XML files (ie, urls) and
  shapes them into a data frame or list of data frames
}
\details{
  \code{urlsToDataFrame} coerces either XML attributes or
  XML values into a data frame. The XML nodes (aka, tags)
  of interest need to be specified as the name(s) of the
  \code{tables} parameter. The values of each \code{tables}
  parameter should be a character vector that defines the
  field names for the respective data frame. These field
  names should match XML attributes or tags.

  When \code{use.values = FALSE}, the length of
  \code{tables} is equal to the number of data frames
  returned and the values of \code{tables} are the fields
  for each data frame. If a particular value of
  \code{tables} is \code{NULL}, the function will
  automatically determine the most complete set of fields
  and fill in \code{NA}s where information is missing. If
  \code{add.children = TRUE}, \code{tables} values should
  be \code{NULL} since child attributes will be used for
  naming convention (with the relevant node as the suffix
  name).

  When \code{use.values = TRUE}, the value(s) of
  \code{tables} are ignored. The XML children of the
  specified node are the fields. If the children are
  inconsistent, missing values are filled with \code{NA}s.
}
\examples{
urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
docs <- urlsToDataFrame(urls)
}

