% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/light2mat.R
\name{light2mat}
\alias{light2mat}
\title{Format light data into a matrix}
\usage{
light2mat(light, twl_offset = 0)
}
\arguments{
\item{light}{data.frame of a \code{tag}, containing at least \code{date} and \code{value}.}

\item{twl_offset}{Shift of the middle of the night compared to 00:00 UTC (in hours). If not
provided, it uses the middle of all nights.}
}
\value{
A data.frame with columns \code{date} and \code{value}.
}
\description{
Format light data into a matrix
}
\concept{twilight}
