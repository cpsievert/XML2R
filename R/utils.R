#' Parse XML Files into XML Documents
#' 
#' Essentially a recursive call to xmlParse.
#' 
#' @importFrom plyr try_default
#' @export
#' @examples
#' urls <- c("http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
#'            "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
#' urls2 <- c("http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346180.xml",
#'            "http://gd2.mlb.com/components/game/mlb/year_2013/mobile/346188.xml")
#' docs <- urlsToDocs(urls)
#' docs2 <- urlsToDocs(urls2)
#' sapply(docs, function(x) attr(x, "XMLsource"))
#' class(docs[[1]])
#' 
#' #better to define a handler?
#' test <- xmlTreeParse(urls[[1]], handlers=list(atbat=f), asTree=FALSE)
            
urlsToDocs <- function(urls){
  docs <- NULL
  for (i in urls) {
    cat(i, "\n")
    doc <- try_default(xmlParse(i), NULL, quiet = TRUE)
    if (!is.null(doc)) {
      attr(doc, "XMLsource") <- i
      docs <- c(docs, doc) #Keep non-empty documents
    }
  }
  return(docs)
}

#' Parse XML Documents into XML Nodes
#' 
#' Essentially a recursive call to getNodeSet.
#' 
#' @export
#' @examples
#' nodes <- docsToNodes(docs, node="atbat")
#' t <- docsToNodes(docs, node="/") #select the root
#' 
#' t2 <- docsToNodes(docs2, node="/highlights/*")

docsToNodes <- function(docs, node) {
  rapply(docs, function(x) getNodeSet(x, node), 
         class=c('XMLInternalDocument', 'XMLAbstractDocument'), how="replace")
}

#' Coerce XML Nodes into a list with both attributes and values
#' 
#' Essentially a recursive call to xmlToList.
#' 
#' @export
#' @examples
#' l <- nodesToList(t)
#' #finds the number of levels in the list -- which should correspond to the number of xmlChildren
#' depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
#' depth(l)
#' l2 <- nodesToList(t2)
#' depth(l2)
#' 

nodesToList <- function(nodes){
  rapply(nodes, function(x) xmlToList(x),
    class=c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode"), how="replace")
}

#' Coerce lists of character vectors into lists of matrices
#' 
#' This function gathers vectors under the same level of the list hierarchy and binds those common vectors into a matrix.
#' In XML terms, this function coerces XML attributes and XML values into a matrix for a particular XML node.
#' 
#' @return A list of matrices. Each element name reflects where in the list hierarchy the information came from.
#' @importFrom plyr rbind.fill.matrix
#' @export
#' @examples
#' l <- nodesToList(t)
#' #finds the number of levels in the list -- which should correspond to the number of xmlChildren
#' depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
#' depth(l)
#' l2 <- nodesToList(t2)
#' depth(l2)
#' 

listsToMatrix <- function(l) {
  #adapted from http://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion?
  squash <- function(x) {
    len <- sum(rapply(x, function(x) 1L))
    y <- vector('list', len)
    i <- 0L
    rapply(x, function(x) { i <<- i+1L; y[[i]] <<- matrix(x, nrow=1); colnames(y[[i]]) <<- names(x) })
    y
  }
  #flatten the nested list hierarchy
  ml <- squash(l)
  
  #http://stackoverflow.com/questions/18862601/extract-name-hierarchy-for-each-leaf-of-a-nested-list
  nms <- names(rapply(l, function(x) if (is.list(x)) name(x) else ""))
  stopifnot(length(ml) == length(nms))
  tapply(ml, INDEX=nms, rbind.fill.matrix)
}



#nl <- rapply(l, function(x) gsub("\\.", "_", x), how="replace")
##worth it??
#if (!identical(nl, l)) warning("Some attribute names contain '.' -- they will be replaced with '_'")
# ul <- unique(names(unlist(l)))
# #remove string that appears after the last period (NOTE: this removes fields names of each table)
# ul2 <- gsub("^(.*)[.].*","\\1", names(ul))
# length(unique(ul2))






#' nodesToData
#' 
#' @export
#' @examples
#' #nodes have three classes?
#' class(nodes[[1]][[1]])
#' attrs <- nodesToData(nodes)

nodesToData <- function(nodes, extract.attributes=TRUE, extract.values=FALSE) { 
  rapply(nodes, function(x) {
                  if (extract.attributes) {
                      y <- xmlAttrs(x)
                      class(y) <- "attribute"
                  }
                  if (extract.values) {
                    z <- xmlValues(x)
                    class(z) <- "value"
                  }
                  return(y)
                }, class="XMLInternalElementNode", how="replace")
  }

#xmlAttrs(nodes[[1]][[1]])
#xmlValue(nodes[[1]][[1]])

#   rapply(nodes, function(x) {
#       y <- xmlValue(x)
#       class(y) <- "value"
#       return(y)
#     }, class="XMLInternalElementNode", how="replace")


#' attrsToDataFrame
#' 
#' @export
#' @examples
#' df <- attrsToDataFrame(attrs)

attrsToDataFrame <- function(attrs, fields) { 
  #might there be a more general way to include both attributes and values here?
  if (is.missing(fields)) {
    fields <- unique(names(unlist(attrs)))
  }
  complete <- rapply(attrs, function(x) adjust(x, tags=fields), class="attribute", how="replace")
  flat.list <- unlist(complete, recursive=FALSE)
  ldply(flat.list)
}

# "Adjust" attributes to match the entire set
# 
# This function adds NAs to missing attributes.
#
# @param info XML attributes from a particular node.
# @param tags "complete" set of attribute names.
# @return returns all present info matching the tags criteria

adjust <- function(info, tags){ #Adds NAs wherever a tag is missing
  x <- names(info)
  y <- tags
  z <- match(x, y)
  w <- z[!is.na(z)] #get rid of elements in info that doesn't match tags (allows fields to be flexible)
  a <- rep(NA, length(tags))
  if (length(w) < length(z)) {
    relevant.info <- info[which(!is.na(z))]
    a[w] <- relevant.info
  } else {
    a[z] <- info
  }
  names(a) <- tags
  return(a)
}
