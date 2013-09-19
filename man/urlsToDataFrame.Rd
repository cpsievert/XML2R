\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into (a list of) matrices or data frame(s)}
\usage{
  urlsToDataFrame(urls, xpath, df = FALSE,
    collapse = "url([0-9]+)\\$")
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

  \item{bind.urls}{logical. Row bind nodes (with common
  ancestors) from different files?}

  \item{collapse}{character vector with node(s) that should
  be merged together regardless of their ancestory}
}
\value{
  Returns list with one element for each relevant XML node.
  Each element contains a matrix by default.
}
\description{
  Parse XML files into (a list of) matrices or data
  frame(s)
}
\examples{
urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/
           gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/
           gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
#collects everything
dat <- urlsToDataFrame(urls)
dat.long <- urlsToDataFrame(urls, bind.urls=FALSE)
names(dat)
dat[[1]] #game level
dat[[2]] #inning level
head(dat[[3]]) #action level
head(dat[[4]]) #atbat level
#collects everything at the atbat level and lower (ie, pitch, runner, etc)
dat.atbat <- urlsToDataFrame(urls, xpath="//atbat")
head(dat.atbat[[1]])



urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
dat3 <- urlsToDataFrame(urls)
}
\seealso{
  \link{urlsToDocs}, \link{docsToNodes},
  \link{nodesToList}, \link{listsToMatrix}
}

