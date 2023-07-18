% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foodweb.R
\name{spanTag}
\alias{spanTag}
\title{spanTag}
\usage{
spanTag(v, direction = "COLSPAN", colorMap)
}
\arguments{
\item{v}{character vector representing first row of m (excluding first 2 entries of m)}

\item{direction}{character string COLSPAN or ROWSPAN}

\item{colorMap}{character array of colors}
}
\value{
returns a list whose components are
\itemize{
\item u return value of unique(v)
\item tab return value of table(v)
\item v2 character vector modified version of v containing html span tags
}
}
\description{
Add html tag for package name to span multiple columns.
Also insert hyperlink to CRAN package and function documentation.
}
\details{
see https://www.pierobon.org/html/span.htm#:~:text=Cells\%20within\%20HTML\%20tables\%20can,span\%20more\%20than\%20one\%20column.
}
\examples{
v<-vector("character")
for(fun in funs_examples)
  v[fun]<-find_funs(fun)$package_name[1]

m<-concatPackFunc2(x_examples$funmat,v)
v2<-consolidate(v)
m2<-rearrangeM(m,v2)

m3<-removeZeroRowsCols(m2)

colors<-c("darkmagenta","darkolivegreen","darkorange3","brown4","red","blue")
colorMap<-mapFunctionsColors(m3[1,c(-1,-2)],m3[c(-1,-2),1],colors)

l<-spanTag(m3[1,c(-1,-2)],"COLSPAN",colorMap)


}