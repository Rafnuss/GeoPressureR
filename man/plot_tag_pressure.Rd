% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.tag.R
\name{plot_tag_pressure}
\alias{plot_tag_pressure}
\title{Plot pressure data of a \code{tag}}
\usage{
plot_tag_pressure(
  tag,
  plot_plotly = TRUE,
  quiet = FALSE,
  warning_pressure_diff = 3,
  warning_stap_length = 12
)
}
\arguments{
\item{tag}{a GeoPressureR \code{tag} object.}

\item{plot_plotly}{logical to use \code{plotly}.}

\item{quiet}{logical to hide warning message about label.}

\item{warning_pressure_diff}{Threshold of pressure hourly difference marking as ️warning (hPa).}

\item{warning_stap_length}{Threshold number of pressure datapoints flagged as ️warning (hourly.}
}
\value{
a plot or ggplotly object.
}
\description{
This function display a plot of pressure time series recorded by a tag
}
\examples{
owd <- setwd(system.file("extdata", package = "GeoPressureR"))
tag <- tag_create("18LX", quiet = TRUE)
setwd(owd)

plot_tag_pressure(tag, plot_plotly = FALSE)

owd <- setwd(system.file("extdata", package = "GeoPressureR"))
tag <- tag_label(tag, quiet = TRUE)
setwd(owd)

plot_tag_pressure(tag)
}
\seealso{
Other plot_tag: 
\code{\link{plot_tag_acceleration}()},
\code{\link{plot_tag_light}()},
\code{\link{plot_tag_temperature}()},
\code{\link{plot_tag_twilight}()}
}
\concept{plot_tag}
