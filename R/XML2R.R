#' Parse XML files into (a list of) matrices or data frame(s)
#' 
#' This function is an experimental wrapper around \link{XML2Obs}. One should only use this function over \link{XML2Obs} if 
#' keys already exist in the XML data and ancestory doesn't need to be altered.
#' 
#' @param urls character vector or list of urls that point to an XML file (or anything readable by \link{xmlParse}).
#' @param xpath XML XPath expression that is passed to \link{getNodeSet}. If missing, the entire root and all descendents are captured and returned (ie, tables = "/"). 
#' @param df logical. Should matrices be coerced into data frames?
#' @return Returns list with one element for each relevant XML node. Each element contains a matrix by default.
#' @seealso \link{urlsToDocs}, \link{docsToNodes}, \link{nodesToList}, \link{listsToObs}
#' @export
#' @examples
#' \dontrun{
#' urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
#'            "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
#' dat3 <- XML2R(urls2)
#' 
#' cens <- "http://www.census.gov/developers/data/sf1.xml"
#' census <- XML2R(cens)
#' }
#' 

XML2R <- function(urls, xpath, df=FALSE) {
  obs <- XML2Obs(urls, xpath)
  #add an option to try to automatically generate keys?
  collapse(obs)
}

#' Parse XML files into a list of "observations"
#' 
#' This function takes a collection of urls that point to XML files and coerces the relevant info into a list of observations.
#' An "observation" is defined as a matrix with one row. An observation can also be thought of as a single instance of 
#' XML attributes (and value) for a particular level in the XML hierarchy. The names of the list reflect the XML node 
#' ancestory for which each observation was extracted from.
#' 
#' It's worth noting that a "url_key" column is appended to each observation to help track the origin of each observation.
#' The value of the "url_key" column is not the actual file name, but a simplified identifier to avoid unnecessarily repeating 
#' long file names for each observation. For this reason, an addition element (named "url_map") is added to the list of observations
#' in case the actual file named want to be used.
#' 
#' @param urls character vector or list of urls that point to an XML file (or anything readable by \link{xmlParse}).
#' @param xpath XML XPath expression that is passed to \link{getNodeSet}. If missing, the entire root and all descendents are captured and returned (ie, tables = "/"). 
#' @param append.value logical. Should the XML value be appended for relevant observations?
#' @param quiet logical. Print file name currently being parsed?
#' @seealso \link{urlsToDocs}, \link{docsToNodes}, \link{nodesToList}, \link{listsToObs}
#' @return A list of "observations" and the "url_map" element. 
#' @export
#' @examples
#' #construct desired file names for first example
#' pre <- "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/"
#' post <- c("gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
#'            "gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
#' urls <- paste0(pre, post)
#' #parse files into a list of observations
#' obs <- XML2Obs(urls)
#' lvls <- unique(names(obs))
#' pitch <- lvls[grep("pitch", lvls)]
#' runner <- lvls[grep("runner", lvls)]
#' action <- lvls[grep("action", lvls)]
#' po <- lvls[grep("po", lvls)]
#' tmp <- rename(obs, equiv=pitch, diff.name="inning") 
#' tmp <- rename(tmp, equiv=runner, diff.name="inning") 
#' tmp <- rename(tmp, equiv=po, diff.name="inning") 
#' tmp <- rename(tmp, equiv=action, diff.name="inning") 
#' obs2 <- rename(tmp, equiv=c("game//inning//top//atbat", "game//inning//bottom//atbat")) 
#' unique(names(obs2))
#' tmp2 <- add_key(obs2, parent="game//inning", key.name="inning_key")
#' obswkey <- add_key(tmp2, parent="game//inning//atbat", key.name="atbat_key")
#' tables <- collapse(obswkey)
#' 
#' #desired file names for second example
#' urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_13/miniscoreboard.xml",
#'            "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/miniscoreboard.xml")
#' obz <- XML2Obs(urls2)
#' unique(names(obz))

XML2Obs <- function(urls, xpath, append.value=TRUE, quiet=FALSE) {
  if (missing(xpath)) xpath <- "/"
  docs <- urlsToDocs(urls, quiet)
  valid.urls <- sapply(docs, function(x) attr(x, "XMLsource"))
  nodes <- docsToNodes(docs, xpath) #select the root
  rm(docs)
  l <- nodesToList(nodes)
  rm(nodes)
  url.count <- paste0("url", seq_len(length(valid.urls)))
  names(valid.urls) <- url.count
  names(l) <- url.count
  obs <- listsToObs(l, append.value)
  rm(l)
  obs[["url_map"]] <- valid.urls
  names(obs) <- sub("//attrs", "", names(obs))
  return(obs)
}
