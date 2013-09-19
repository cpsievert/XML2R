\name{nodesToList}
\alias{nodesToList}
\title{Coerce XML Nodes into a list with both attributes and values}
\usage{
  nodesToList(nodes)
}
\arguments{
  \item{nodes}{A collection of XML nodes. Should be the
  output from \link{docsToNodes}.}
}
\description{
  Essentially a recursive call to \link{xmlToList}.
}

