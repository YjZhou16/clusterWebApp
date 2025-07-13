#' Plot Silhouette Diagram
#'
#' Plots the silhouette diagram for a given clustering result.
#'
#' @param sil A silhouette object as returned by \code{\link[cluster]{silhouette}}.
#'
#' @return A silhouette plot if input is not NULL, otherwise a placeholder text.
#'
#' @importFrom factoextra fviz_silhouette
#' @importFrom graphics plot.new text
#' @importFrom cluster silhouette
#' @examples
#' data <- scale(iris[, 1:4])
#' cl <- kmeans(data, 3)$cluster
#' sil <- cluster::silhouette(cl, dist(data))
#' if (interactive()) {
#'   plot_silhouette(sil)
#' }
#'
#'
#' @export
plot_silhouette <- function(sil) {
  if (!is.null(sil)) {
    fviz_silhouette(sil)
  } else {
    plot.new()
    text(0.5, 0.5, "Silhouette plot not available")
  }
}
