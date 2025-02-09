% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.tag.R
\name{plot_tag_twilight}
\alias{plot_tag_twilight}
\title{Plot twilight data of a \code{tag}}
\usage{
plot_tag_twilight(
  tag,
  twilight_line = NULL,
  transform_light = TRUE,
  twl_offset = NULL,
  plot_plotly = FALSE
)
}
\arguments{
\item{tag}{a GeoPressureR \code{tag} object}

\item{twilight_line}{a twilight data.frame typically created with \code{path2twilight()} which is
displayed as a line}

\item{transform_light}{logical to use a log transformation of light}

\item{twl_offset}{Shift of the middle of the night compared to 00:00 UTC (in hours). If not
provided, it uses the middle of all nights.}

\item{plot_plotly}{logical to use \code{plotly}}
}
\value{
a plot object.
}
\description{
This function display a plot of twilight time series recorded by a tag
}
\examples{
withr::with_dir(system.file("extdata", package = "GeoPressureR"), {
  tag <- tag_create("18LX", quiet = TRUE)

  plot_tag_twilight(tag, plot_plotly = TRUE)

  tag <- tag_label(tag, quiet = TRUE)

  plot_tag_twilight(tag)
})
}
\seealso{
Other plot_tag: 
\code{\link{plot_tag_acceleration}()},
\code{\link{plot_tag_light}()},
\code{\link{plot_tag_pressure}()},
\code{\link{plot_tag_temperature}()}
}
\concept{plot_tag}
