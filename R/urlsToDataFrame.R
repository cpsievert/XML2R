#' Parse XML files into (a list of) matrices or data frame(s)
#' 
#' 
#' 
#' @param urls set of urls for parsing
#' @param xpath XML XPath expression that is passed to \link{getNodeSet}. If missing, the entire root and all descendents are captured and returned (ie, tables = "/"). 
#' @param df logical. Should matrices be coerced into data frames?
#' @return Returns list with one element for each relevant XML node. Each element contains a matrix by default.
#' @export
#' @examples
#' urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
#'            "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
#' dat <- urlsToDataFrame(urls) #collects everything
#' dat2 <- urlsToDataFrame(urls, xpath="//atbat") #collects everything at the atbat level and lower (ie, pitch, runner, etc)
#' 

urlsToDataFrame <- function(urls, xpath, df=FALSE) {
  if (missing(xpath)) xpath <- "/"
  docs <- urlsToDocs(urls)
  valid.urls <- sapply(docs, function(x) attr(x, "XMLsource"))
  nodes <- docsToNodes(docs, node=xpath) #select the root
  rm(docs)
  l <- nodesToList(nodes)
  rm(nodes)
  m <- listsToMatrix(l)
  rm(l)
  if (df) m <- lapply(m, function(x) data.frame(x, stringsAsFactors=FALSE))
  return(m)
}
