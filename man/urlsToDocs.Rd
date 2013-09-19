\name{urlsToDocs}
\alias{urlsToDocs}
\title{Parse XML Files into XML Documents}
\usage{
  urlsToDocs(urls)
}
\arguments{
  \item{urls}{character vector or list of urls that point
  to an XML file (or anything readable by
  \link{xmlParse}).}
}
\description{
  Essentially a recursive call to \link{xmlParse}.
}

