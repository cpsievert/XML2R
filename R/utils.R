#' Parse XML Files into XML Documents
#' 
#' Essentially a recursive call to \link{xmlParse}.
#' 
#' @param urls character vector or list of urls that point to an XML file (or anything readable by \link{xmlParse}).
#' @param quiet logical. Print file name currently being parsed?
#' @importFrom plyr try_default
#' @importFrom XML xmlParse
#' @export
            
urlsToDocs <- function(urls, quiet=FALSE){
  docs <- NULL
  for (i in urls) {
    if (!quiet) cat(i, "\n")
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
#' @return A nested list with a structure that resembles the XML structure
#' @export
#' 

nodesToList <- function(nodes){
  #I should really figure which class I want...
  rapply(nodes, function(x) xmlToList(x),
    classes=c("XMLInternalElementNode", "XMLInternalNode", "XMLAbstractNode"), how="replace")
}

#' Flatten nested list into a list of observations
#' 
#' This function flattens the nested list into a list of "observations" (that is, a list of matrices with one row).
#' The names of the list that is returned reflects the XML ancestory of each observation.
#' 
#' @param l list. Should be the output from \link{nodesToList}. 
#' @param append.value logical. Should the XML value be appended to the observation?
#' @return A list where each element reflects one "observation".
#' @export

#adapted from knowledege gained from here:
#http://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion?
##http://stackoverflow.com/questions/18862601/extract-name-hierarchy-for-each-leaf-of-a-nested-list

listsToObs <- function(l, append.value=TRUE) {
  #Assuming the names of the list elements holding XML values (and only values) will be NULL, we can distinguish between values/attributes
  #By also assuming that values appear immediately before their respective attributes (which appears to be the way xmlToList works),
  #we append the XML value to a row of attributes (given that their from the same node)
  name.len <- rapply(l, function(x) length(names(x)))  
  list.len <- length(name.len)
  nms <- names(name.len)
  #if (is.null(names(l[[1]]))) nms <- rep(names(l), sapply(l, length)) #overwrite names if no names (or no children) exist
  # turn '..attr' into '.attr' -- note it can show up multiple times in a name
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
  #Append XML_value column to the appropriate attributes
  #Note that this assumes the value always appears before the attributes in the list order
  if (append.value && length(indicies) > 0) {
    values <- holder[indicies]
    add.values <- holder[indicies+1]
    holder[indicies+1] <- mapply(function(x, y){ cbind(x, y) }, add.values, values, SIMPLIFY=FALSE)
    holder[indicies] <- NULL
    #remove the XML value elements (so they aren't duplicated)
    urls <- urls[-indicies]
    node.sets <- node.sets[-indicies]
  }
  holder <- mapply(function(x, y) cbind(x, url_key=y), holder, urls, SIMPLIFY=FALSE)
  names(holder) <- node.sets
  return(holder)
}


#' Rename rows of a list
#' 
#' This function takes a list of "observations" (that is, a list of matrices with one row) and 
#' changes the names of that list. The names are intended to reflect the XML node ancestory for which 
#' the observation was extracted from. Sometimes, certain nodes in an XML ancestory may want to be neglected
#' before any keys are created (see \link{add_key}) or observations are aggregated (see \link{collapse}).
#' Note that neglected nodes are saved in the "neglected" attribute of any observation whose name was overwritten.
#' 
#' @param obs list. Should be the output from \link{listsToObs}. 
#' @param equiv character vector with the appropriate (unique) names from \code{r} that should be regarded "equivalent".
#' @param diff.name character string used for naming the variable that is appended to any observations whose name was overwritten. 
#' The value for this variable is the difference in from the original name and the overwritten name.
#' @return A list of "observations". 
#' @export

rename <- function(obs, equiv, diff.name="diff_name"){
  if (missing(equiv)) return(obs)
  nms <- names(obs)
  if (is.null(nms)) warning("The observations don't not have any names!")
  if (all(!equiv %in% unique(nms))) warning("None of the equiv elements match the names of the observations.")
  baseline <- strsplit(equiv, "//")
  n <- length(equiv)
  keeps <- baseline[[1]]
  names(baseline) <- equiv
  for (i in seq_len(n-1)) { #get all the common nodes
    idx <- keeps %in% baseline[[i+1]]
    keeps <- keeps[idx]
  }
  #construct the 'label' to replace all the names that match equiv
  if (length(keeps) == 0) {
    warning("None of the XML node names seem to match. Are you sure you want to regard these names equivalent?")
    label <- "equivalent"
  } else {
    label <- paste0(keeps, collapse="//") 
  }
  message(paste0("Renaming all list elements named: \n", paste(equiv, collapse="  OR  "), "\nwith\n", label))
  diffs <- lapply(baseline, function(x) paste(x[!x %in% keeps], collapse="//")) #keeps the nodes that will be 'overwritten'
  idx <- nms %in% equiv
  names(obs)[idx] <- label #overwrite the names
  klass <- as.character(diffs[match(nms[idx], names(diffs))]) #get the classes for the objects whose name was overwritten
  obs[idx] <- mapply(function(x, y) cbind(x, `colnames<-`(cbind(y), diff.name)), obs[idx], klass, SIMPLIFY=FALSE)
  return(obs)
  #old code
  #base <- strsplit(rep(equiv[1], length(equiv[-1])), "//")
  #compare <- strsplit(equiv[-1], "//")
  #diffs <- mapply(function(x, y) y[x != y], base, compare) #tags removed from common name
  #sames <- mapply(function(x, y) y[x == y], base, compare) #common names
  #if (length(diffs) > 1) diffs <- paste0(diffs, collaspe="//")
}

#' Add a key to connect observations
#' 
#' This function takes a list of "observations" (that is, a list of matrices with one row) and appends a new column to 
#' each relevant observation. This column is a key that connects \code{parent}s to \code{child}ren in case their 
#' observations need to be joined together at a later point. 
#' 
#' @param obs list. Should be the output from \link{listsToObs}. 
#' @param parent character string. Should be present in the names of \code{rows}.
#' @param child character string. Should be present in the names of \code{rows}.
#' @param key.name The desired column name of the newly generated key.
#' @return A list of "observations".
#' @export

add_key <- function(obs, parent, child, key.name="key_name"){
  if (missing(parent)) stop("You must provide the parent argument!")
  nms <- names(obs)
  un <- unique(nms)
  if (!parent %in% un) warning("The parent argument you provided does not match any observations.")
  if (missing(child)){
    fetus <- un[-which(un == parent)]
    children <- fetus[grep(paste0(parent, "//.*"), fetus)]
    if (length(children) == 0){
      stop(paste0("No children were found for the ", parent, " node."))
    } else {
      message(paste0("A key for the following children will be generated for the ", parent, " node:\n", 
                     paste0(children, collapse="\n")))
    }
  } else {
    children <- child
  }
  #first add parent (or outer) index
  elders <- nms == parent
  outer_index <- seq_len(sum(elders))
  obs[elders] <- mapply(function(x, y) cbind(x, `colnames<-`(cbind(y), key.name)), obs[elders], outer_index, SIMPLIFY=FALSE) 
  #now add the (inner) index for the children
  for (child in children) {
    kids <- nms == child
    kid.idx <- which(kids)
    relevant <- obs[elders | kids] #have to subset to relevant cases to get the proper indexing
    elder.idx <- which(names(relevant) == parent)
    timez <- diff(c(0, elder.idx))-1 #yields the number of children between each parent
    inner_index <- rep(outer_index, timez)
    obs[kid.idx] <- mapply(function(x, y) cbind(x, `colnames<-`(cbind(y), key.name)), obs[kid.idx], inner_index, SIMPLIFY=FALSE) 
  }
  return(obs)
}

#' Collapse a list of observations into a list of tables.
#' 
#' This function aggregates all observations with a similar name into a common table. Note that observations 
#' with a particular name don't need consistent variables (any missing information is filled with NAs).
#' 
#' @param obs list of observations.
#' @return Returns list with one element for each relevant XML node. Each element contains a matrix.
#' @importFrom plyr rbind.fill.matrix
#' @export

collapse <- function(obs) {
  nms <- names(obs)
  map <- grep("url_map", nms)
  url.map <- obs[map]
  obs <- obs[-map]
  obs.nms <- nms[-map]
  return(tapply(obs, INDEX=obs.nms, rbind.fill.matrix))
}
