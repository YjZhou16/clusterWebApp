#' Plot Elbow Method for KMeans
#'
#' Uses within-cluster sum of squares (WSS) to help determine the optimal number of clusters.
#'
#' @param data A numeric matrix or data frame for clustering.
#'
#' @return A ggplot object showing the elbow plot.
#'
#' @importFrom stats kmeans
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_minimal labs
#'
#' @examples
#' data <- scale(iris[, 1:4])
#' if (interactive()) {
#'   plot_elbow(data)
#' }
#'
#'
#' @export
plot_elbow <- function(data) {
  wss <- sapply(1:10, function(k) kmeans(data, centers = k)$tot.withinss)
  df <- data.frame(k = 1:10, wss = wss)
  ggplot(df, aes(k, wss)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Elbow Method", x = "Number of Clusters", y = "Total WSS")
}

