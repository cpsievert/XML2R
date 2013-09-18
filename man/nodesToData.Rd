\name{nodesToData}
\alias{nodesToData}
\title{nodesToData}
\usage{
  nodesToData(nodes, extract.attributes = TRUE,
    extract.values = FALSE)
}
\description{
  nodesToData
}
\examples{
#nodes have three classes?
class(nodes[[1]][[1]])
attrs <- nodesToData(nodes)
}

