\name{nodesToAttr}
\alias{nodesToAttr}
\title{nodesToAttr}
\usage{
  nodesToAttr(nodes, add.children = FALSE)
}
\description{
  nodesToAttr
}
\examples{
#nodes have three classes?
class(nodes[[1]][[1]])
attrs <- nodesToAttr(nodes)
}

