% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_path.R
\name{plot_path}
\alias{plot_path}
\title{Plot a \code{path}}
\usage{
plot_path(
  path,
  plot_leaflet = TRUE,
  provider = "Esri.WorldTopoMap",
  provider_options = leaflet::providerTileOptions(),
  pad = 3,
  ...
)
}
\arguments{
\item{path}{a GeoPressureR \code{path} data.frame.}

\item{plot_leaflet}{logical defining if the plot is an interactive \code{leaflet} map or a static
basic plot.}

\item{provider}{the name of the provider (see
\url{https://leaflet-extras.github.io/leaflet-providers/preview/} and
\url{https://github.com/leaflet-extras/leaflet-providers})}

\item{provider_options}{tile options. See leaflet::addProviderTiles() and
leaflet::providerTileOptions()}

\item{pad}{padding of the map in degree lat-lon (only for \code{plot_leaflet = FALSE}).}

\item{...}{additional parameters for \code{plot_path_leaflet()}}
}
\value{
modified map object
}
\description{
This function plots a \code{path} data.frame. This function is used in \code{\link[=plot.map]{plot.map()}}.
}
\examples{
setwd(system.file("extdata", package = "GeoPressureR"))
tag <- tag_create("18LX", quiet = TRUE) |>
  tag_label(quiet = TRUE) |>
  tag_set_map(c(-16, 23, 0, 50), scale = 1)
path <- ind2path(c(1652, 1603, 1755, 1708, 1607), tag)

plot_path(path)

}
\seealso{
\code{\link[=plot.map]{plot.map()}}

Other path: 
\code{\link{ind2path}()},
\code{\link{path2edge}()},
\code{\link{path2elevation}()}
}
\concept{path}
