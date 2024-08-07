% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_simulation.R
\name{graph_simulation}
\alias{graph_simulation}
\title{Simulate randomly multiple trajectories}
\usage{
graph_simulation(graph, nj = 10, quiet = FALSE)
}
\arguments{
\item{graph}{a graph object.}

\item{nj}{Number of simulations.}

\item{quiet}{logical to hide messages about the progress.}
}
\value{
Path data.frame containing the columns
-\code{stap_id} stationary period
\itemize{
\item \code{j} unique ID for each simulation.
\item \code{ind} indices of the coordinate in the 2D grid. Useful to retrieve map or graph information
\item \code{lat} latitude,
\item \code{lon} longitude
\item \code{start} datetime of the start of the stationary period (same as in \code{stap})
\item \code{end} datetime of the end of the stationary period (same as in \code{stap})
\item \code{include} logical if stationary period was modelled (same as in \code{stap})
\item \verb{nb_sample known} number of datapoint used to compute pressure (same as in \code{stap})
}
}
\description{
This function randomly simulates multiple trajectories from a graph using the forward filtering
backward sampling algorithm. For more
details, see \href{https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14082#mee314082-sec-0013-title}{section 2.3.3 of Nussbaumer et al. (2023b)}
and the \href{https://bit.ly/3YE83Wn}{GeoPressureManual}.
}
\examples{
owd <- setwd(system.file("extdata", package = "GeoPressureR"))
tag <- tag_create("18LX", quiet = TRUE) |>
  tag_label(quiet = TRUE) |>
  twilight_create() |>
  twilight_label_read() |>
  tag_set_map(
    extent = c(-16, 23, 0, 50),
    known = data.frame(stap_id = 1, known_lon = 17.05, known_lat = 48.9)
  ) |>
  geopressure_map(quiet = TRUE) |>
  geolight_map(quiet = TRUE)
setwd(owd)

# Create graph
graph <- graph_create(tag, quiet = TRUE)

# Define movement model
graph <- graph_set_movement(graph)

# Compute simulations
path_simulation <- graph_simulation(graph, quiet = TRUE)

plot_path(path_simulation, plot_leaflet = FALSE)

}
\references{
{ Nussbaumer, Raphaël, Mathieu Gravey, Martins Briedis, Felix Liechti, and Daniel
Sheldon. 2023. Reconstructing bird trajectories from pressure and wind data using a highly
optimized hidden Markov model. \emph{Methods in Ecology and Evolution}, 14, 1118–1129
\url{https://doi.org/10.1111/2041-210X.14082}.}
}
\seealso{
\href{https://bit.ly/3YE83Wn}{GeoPressureManual}

Other graph: 
\code{\link{graph_create}()},
\code{\link{graph_marginal}()},
\code{\link{graph_most_likely}()},
\code{\link{graph_set_movement}()},
\code{\link{print.graph}()}
}
\concept{graph}
