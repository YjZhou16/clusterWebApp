#' Compute Average Silhouette Width
#'
#' Calculates the average silhouette coefficient from a silhouette object.
#'
#' @param sil A silhouette object as returned by \code{\link[cluster]{silhouette}}.
#'
#' @return A numeric value indicating the average silhouette width, or \code{NA} if input is \code{NULL}.
#'
#' @examples
#' data <- scale(iris[, 1:4])
#' cl <- kmeans(data, 3)$cluster
#' sil <- cluster::silhouette(cl, dist(data))
#' if (interactive()) {
#'   compute_silhouette(sil)
#' }
#'
#'
#' @export
compute_silhouette <- function(sil) {
  if (!is.null(sil)) mean(sil[, 3]) else NA
}
