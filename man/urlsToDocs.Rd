\name{urlsToDocs}
\alias{urlsToDocs}
\title{Parse XML Files into XML Documents}
\usage{
urlsToDocs(urls, quiet = FALSE)
}
\arguments{
  \item{urls}{character vector or list of urls that point
  to an XML file (or anything readable by
  \link{xmlParse}).}

  \item{quiet}{logical. Print file name currently being
  parsed?}
}
\description{
Essentially a recursive call to \link{xmlParse}.
}

