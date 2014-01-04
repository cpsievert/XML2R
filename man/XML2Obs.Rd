\name{XML2Obs}
\alias{XML2Obs}
\title{Parse XML files into a list of "observations"}
\usage{
  XML2Obs(urls, xpath, append.value = TRUE,
    as.equiv = TRUE, quiet = FALSE)
}
\arguments{
  \item{urls}{character vector or list of urls that point
  to an XML file (or anything readable by
  \link{xmlParse}).}

  \item{xpath}{XML XPath expression that is passed to
  \link{getNodeSet}. If missing, the entire root and all
  descendents are captured and returned (ie, tables =
  "/").}

  \item{append.value}{logical. Should the XML value be
  appended for relevant observations?}

  \item{as.equiv}{logical. Should observations from two
  different files (but the same ancestory) have the same
  name returned?}

  \item{quiet}{logical. Print file name currently being
  parsed?}
}
\value{
  A list of "observations" and the "url_map" element.
}
\description{
  This function takes a collection of urls that point to
  XML files and coerces the relevant info into a list of
  observations. An "observation" is defined as a matrix
  with one row. An observation can also be thought of as a
  single instance of XML attributes (and value) for a
  particular level in the XML hierarchy. The names of the
  list reflect the XML node ancestory for which each
  observation was extracted from.
}
\details{
  It's worth noting that a "url_key" column is appended to
  each observation to help track the origin of each
  observation. The value of the "url_key" column is not the
  actual file name, but a simplified identifier to avoid
  unnecessarily repeating long file names for each
  observation. For this reason, an addition element (named
  "url_map") is added to the list of observations in case
  the actual file named want to be used.
}
\examples{
#construct desired file names for first example
pre <- "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/"
post <- c("gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
           "gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
urls <- paste0(pre, post)
#parse files into a list of observations
obs <- XML2Obs(urls)
lvls <- unique(names(obs))
pitch <- lvls[grep("pitch", lvls)]
runner <- lvls[grep("runner", lvls)]
action <- lvls[grep("action", lvls)]
po <- lvls[grep("po", lvls)]
tmp <- re_name(obs, equiv=pitch, diff.name="inning")
tmp <- re_name(tmp, equiv=runner, diff.name="inning")
tmp <- re_name(tmp, equiv=po, diff.name="inning")
tmp <- re_name(tmp, equiv=action, diff.name="inning")
obs2 <- re_name(tmp, equiv=c("game//inning//top//atbat", "game//inning//bottom//atbat"))
unique(names(obs2))
tmp2 <- add_key(obs2, parent="game//inning", key.name="inning_key")
obswkey <- add_key(tmp2, parent="game//inning//atbat", key.name="atbat_key")
tables <- collapse(obswkey)

#desired file names for second example
urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_13/miniscoreboard.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/miniscoreboard.xml")
obz <- XML2Obs(urls2)
unique(names(obz))
}
\seealso{
  \link{urlsToDocs}, \link{docsToNodes},
  \link{nodesToList}, \link{listsToObs}
}

