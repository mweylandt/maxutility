% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/others.R
\name{get_charcount}
\alias{get_charcount}
\title{Returns a vector of character counts}
\usage{
get_charcount(x, text_col)
}
\arguments{
\item{text_col}{text column to be counted}
}
\description{
Returns a vector of character counts
}
\examples{
testdf <- data.frame(author=c("me", "you"), text = c("democracy is so nice", "democratic movements happen en la realidad"))
get_wordcount(testdf, text=text)
}
