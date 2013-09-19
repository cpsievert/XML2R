#' Parse XML files into (a list of) matrices or data frame(s)
#' 
#' The main function of the XML2R package. Documentation to come...
#' 
#' @param urls character vector or list of urls that point to an XML file (or anything readable by \link{xmlParse}).
#' @param xpath XML XPath expression that is passed to \link{getNodeSet}. If missing, the entire root and all descendents are captured and returned (ie, tables = "/"). 
#' @param df logical. Should matrices be coerced into data frames?
#' @return Returns list with one element for each relevant XML node. Each element contains a matrix by default.
#' @seealso \link{urlsToDocs}, \link{docsToNodes}, \link{nodesToList}, \link{listsToMatrix}
#' @export
#' @examples
#' \dontrun{
#' pre <- "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/"
#' post <- c("gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
#'            "gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
#' urls <- paste0(pre, post)
#' #collects everything
#' dat <- XML2R(urls) 
#' names(dat)
#' dat[[1]] #game level
#' dat[[2]] #inning level
#' head(dat[[3]]) #action level
#' head(dat[[4]]) #atbat level
#' #collects everything at the atbat level and lower (ie, pitch, runner, etc)
#' dat.atbat <- XML2R(urls, xpath="//atbat") 
#' head(dat.atbat[[1]])
#' 
#' urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
#'            "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
#' dat3 <- XML2R(urls)
#' }
#' 

XML2R <- function(urls, xpath, df=FALSE) {
  if (missing(xpath)) xpath <- "/"
  docs <- urlsToDocs(urls)
  valid.urls <- sapply(docs, function(x) attr(x, "XMLsource"))
  nodes <- docsToNodes(docs, xpath) #select the root
  rm(docs)
  l <- nodesToList(nodes)
  rm(nodes)
  #since listsToMatrix "collapses" unnamed lists, we now add url identifiers to the first level of elements
  url.count <- paste0("url", seq_len(length(valid.urls)))
  names(valid.urls) <- url.count
  names(l) <- url.count
  m <- listsToMatrix(l)
  rm(l)
  if (df) m <- lapply(m, function(x) data.frame(x, stringsAsFactors=FALSE))
#   nms <- names(m)
#   idx <- gsub(collapse, "", nms)
#   m <- tapply(m, INDEX=idx, rbind.fill.matrix)
  
  m[["url_map"]] <- valid.urls
  return(m)
}

#' Collapse a list of matrices (or data frames) into a smaller list of larger matrices or data frames
#' 
#' @param m list of matrices or data frames
#' @param condition A partial match of names(m). 
#' @return Returns list with one element for each relevant XML node. Each element contains a matrix by default.
#' @importFrom plyr rbind.fill.matrix
#' @export

collapse <- function(m, condition="pitch") {
  #collapse list of matrices according to some type of syntax
  nms <- names(m)
  nmz <- nms[grep(condition, nms)]
  browser()
  nm.split <- strsplit(nmz, split="\\$")
  idx <- sapply(nm.split, function(x) x[grep(condition, x)])
  return(tapply(m, INDEX=idx, rbind.fill.matrix))
  #cond <- paste0(".*\\$$", condition)
  #gsub(".*\\$$")
  #i <- 0L
  #lapply(m, function(x) { i <<- i+1L; x$source <- names(m)[[i]] })
}
