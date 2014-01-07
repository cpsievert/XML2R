\name{XML2R}
\alias{XML2R}
\title{Parse XML files into (a list of) matrices or data frame(s)}
\usage{
XML2R(urls, xpath, df = FALSE)
}
\arguments{
  \item{urls}{character vector or list of urls that point
  to an XML file (or anything readable by
  \link{xmlParse}).}

  \item{xpath}{XML XPath expression that is passed to
  \link{getNodeSet}. If missing, the entire root and all
  descendents are captured and returned (ie, tables =
  "/").}

  \item{df}{logical. Should matrices be coerced into data
  frames?}
}
\value{
Returns list with one element for each relevant XML node.
Each element contains a matrix by default.
}
\description{
This function is an experimental wrapper around
\link{XML2Obs}. One should only use this function over
\link{XML2Obs} if keys already exist in the XML data and
ancestory doesn't need to be altered.
}
\examples{
\dontrun{
urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
dat3 <- XML2R(urls2)

cens <- "http://www.census.gov/developers/data/sf1.xml"
census <- XML2R(cens)
}
}
\seealso{
\link{urlsToDocs}, \link{docsToNodes}, \link{nodesToList},
\link{listsToObs}
}

