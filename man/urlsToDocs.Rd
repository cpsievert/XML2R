\name{urlsToDocs}
\alias{urlsToDocs}
\title{Parse XML Files into XML Documents}
\usage{
  urlsToDocs(urls)
}
\description{
  Essentially a recursive call to xmlParse.
}
\examples{
urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
           "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
docs <- urlsToDocs(urls)
docs2 <- urlsToDocs(urls2)
sapply(docs, function(x) attr(x, "XMLsource"))
class(docs[[1]])

#better to define a handler?
test <- xmlTreeParse(urls[[1]], handlers=list(atbat=f), asTree=FALSE)
}

