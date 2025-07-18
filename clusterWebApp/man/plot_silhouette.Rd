% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_silhouette.R
\name{plot_silhouette}
\alias{plot_silhouette}
\title{Plot Silhouette Diagram}
\usage{
plot_silhouette(sil)
}
\arguments{
\item{sil}{A silhouette object as returned by \code{\link[cluster]{silhouette}}.}
}
\value{
A silhouette plot if input is not NULL, otherwise a placeholder text.
}
\description{
Plots the silhouette diagram for a given clustering result.
}
\examples{
data <- scale(iris[, 1:4])
cl <- kmeans(data, 3)$cluster
sil <- cluster::silhouette(cl, dist(data))
if (interactive()) {
  plot_silhouette(sil)
}


}
