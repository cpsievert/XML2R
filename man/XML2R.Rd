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
  The main function of the XML2R package. Documentation to
  come...
}
\examples{
\dontrun{
pre <- "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/"
post <- c("gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
           "gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
urls <- paste0(pre, post)
#collects everything
dat <- XML2R(urls)
names(dat)
dat[[1]] #game level
dat[[2]] #inning level
head(dat[[3]]) #action level
head(dat[[4]]) #atbat level
#collects everything at the atbat level and lower (ie, pitch, runner, etc)
dat.atbat <- XML2R(urls, xpath="//atbat")
head(dat.atbat[[1]])

urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
dat3 <- XML2R(urls)
}
}
\seealso{
  \link{urlsToDocs}, \link{docsToNodes},
  \link{nodesToList}, \link{listsToMatrix}
}

