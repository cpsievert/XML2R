#' Parse XML Files into XML Documents
#' 
#' Essentially a recursive call to \link{xmlParse}.
#' 
#' @param urls character vector or list of urls that point to an XML file (or anything readable by \link{xmlParse}).
#' @importFrom plyr try_default
#' @importFrom XML xmlParse
#' @export
            
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
#' Essentially a recursive call to \link{getNodeSet}.
#' 
#' @param docs XML documents
#' @param xpath xpath expression
#' @importFrom XML getNodeSet
#' @export

docsToNodes <- function(docs, xpath) {
  #I should really figure which class I want...
  rapply(docs, function(x) getNodeSet(x, xpath), 
         classes=c('XMLInternalDocument', 'XMLAbstractDocument'), how="replace")
}


#' Coerce XML Nodes into a list with both attributes and values
#' 
#' Essentially a recursive call to \link{xmlToList}.
#' 
#' @param nodes A collection of XML nodes. Should be the output from \link{docsToNodes}.
#' @importFrom XML xmlToList
#' @export
#' 

nodesToList <- function(nodes){
  #I should really figure which class I want...
  rapply(nodes, function(x) xmlToList(x),
    classes=c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode"), how="replace")
}

#' Coerce lists of character vectors into lists of matrices
#' 
#' This function gathers vectors under the same level of the list hierarchy and binds those common vectors into a matrix.
#' In XML terms, this function coerces XML attributes and XML values into a matrix for a particular XML node.
#' 
#' @param l list. Should be the output from \link{nodesToList}. 
#' @return A list of matrices. Each element name reflects where in the list hierarchy the information came from.
#' @importFrom plyr rbind.fill.matrix
#' @importFrom plyr amv_dimnames
#' @export

listsToMatrix <- function(l) {
  #adapted from knowledege gained from here:
  #http://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion?
  ##http://stackoverflow.com/questions/18862601/extract-name-hierarchy-for-each-leaf-of-a-nested-list

  #Assuming the names of the list elements holding XML values (and only values) will be NULL, we can distinguish between values/attributes
  #By also assuming that values appear immediately before their respective attributes (which appears to be the way xmlToList works),
  #we append the XML value to a row of attributes (given that their from the same node)
  name.len <- rapply(l, function(x) length(names(x)))  
  nms <- names(name.len)
  list.len <- length(name.len)
  # turn '..attr' into '.attr' -- note it show up multiple times in a name
  temp <- gsub('.attrs', 'attrs', fixed=TRUE, nms)
  idx <- gsub('.', '//', fixed=TRUE, temp)
  #select the url prefix and replace with nothing
  node.sets <- sub("url([0-9]+)//", "", idx)
  urls <- sub("//.*$", "", idx)
  suffix <- sub(".*//", "", node.sets)
  indicies <- which(name.len == 0 & suffix %in% "text") #tracks which XML values should be appended to the sequential row
  
  #placeholder for the flattened list hierarchy
  holder <- vector('list', list.len)
  i <- 0L
  #fill up placeholder with relevant info
  rapply(l, function(x) { 
              i <<- i+1L 
              holder[[i]] <<- matrix(x, nrow=1)
              nmz <- names(x)
              if (is.null(nmz)) nmz <- "XML_value"
              colnames(holder[[i]]) <<- nmz
            })
  #Append XML_value column to the appropriate attributes, then remove the XML value elements
  #Note that this assumes the value always appears before the attributes in the list order
  if (length(indicies) > 0) {
    values <- holder[indicies]
    add.values <- holder[indicies+1]
    holder[indicies+1] <- mapply(function(x, y){ cbind(x, y) }, add.values, values, SIMPLIFY=FALSE)
    holder[indicies] <- NULL
    urls <- lapply(urls[-indicies], function(x){
      m <- matrix(x, nrow=1, ncol=1)
      colnames(m) <- "URL_source"
      m
    })
    node.sets <- node.sets[-indicies]
  }
  holder <- mapply(function(x, y) cbind(x, y), holder, urls, SIMPLIFY=FALSE)
  tapply(holder, INDEX=node.sets, rbind.fill.matrix)
}

#stopifnot(sum(counts) == length(elements))
#Here we assume that XML values ALWAYS terminate an XML node.
#The benefit is that we can treat XML values as another attribute of a node (which let's us treat everything as one row/observation).
#   names(elements) <- gsub("\\.text$", "\\.XML_value", names(elements))
#   idx <- names(counts)
#   temp <- sub("\\.text$", "", idx)
#   idx <- sub("\\.text", "", temp)
#   nms <- rapply(l, names)
#   len <- length(counts)
#   nms2 <- names(nest)

#nl <- rapply(l, function(x) gsub("\\.", "_", x), how="replace")
##worth it??
#if (!identical(nl, l)) warning("Some attribute names contain '.' -- they will be replaced with '_'")
# ul <- unique(names(unlist(l)))
# #remove string that appears after the last period (NOTE: this removes fields names of each table)
# ul2 <- gsub("^(.*)[.].*","\\1", names(ul))
# length(unique(ul2))



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
