% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foodweb.R
\name{replaceRotTag}
\alias{replaceRotTag}
\title{replaceRotTag}
\usage{
replaceRotTag(x, l, dims)
}
\arguments{
\item{x}{return value of readLines(), HTML code containing data table}

\item{l}{return values of spanTag()}

\item{dims}{return value of dim()}
}
\value{
returns modified version of HTML code containing data table
}
\description{
insert html tags for rotating text
}
\examples{
if(interactive()){
load("data/x_x.RData")
load("data/x_l.RData")
load("data/x_m3.RData")
x<-replaceRotTag(x_x,x_l,dim(x_m3))
}

}
