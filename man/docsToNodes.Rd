\name{docsToNodes}
\alias{docsToNodes}
\title{Parse XML Documents into XML Nodes}
\usage{
  docsToNodes(docs, node)
}
\description{
  Essentially a recursive call to getNodeSet.
}
\examples{
nodes <- docsToNodes(docs, node="atbat")
t <- docsToNodes(docs, node="/") #select the root

t2 <- docsToNodes(docs2, node="/highlights/*")
}

