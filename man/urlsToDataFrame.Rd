\name{urlsToDataFrame}
\alias{urlsToDataFrame}
\title{Parse XML files into (a list of) matrices or data frame(s)}
\usage{
  urlsToDataFrame(urls, xpath, df = FALSE)
}
\arguments{
  \item{urls}{set of urls for parsing}

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
  Parse XML files into (a list of) matrices or data
  frame(s)
}
\examples{
urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
dat <- urlsToDataFrame(urls) #collects everything
dat2 <- urlsToDataFrame(urls, xpath="//atbat") #collects everything at the atbat level and lower (ie, pitch, runner, etc)
}

