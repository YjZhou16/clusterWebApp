#' Plot Radar Chart for PAM Cluster Centers
#'
#' Displays the medoids of each PAM cluster using a polar radar chart.
#'
#' @param data A numeric matrix or data frame for clustering.
#' @param clusters An integer indicating the number of clusters.
#'
#' @return A ggplot object showing the radar chart of cluster medoids.
#'
#' @importFrom cluster pam
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point coord_polar theme_minimal labs
#'
#' @examples
#' data <- scale(iris[, 1:4])
#' if (interactive()) {
#'   plot_radar(data, clusters = 3)
#' }
#'
#'
#' @export
plot_radar <- function(data, clusters) {
  pam_res <- pam(data, k = clusters)
  centers <- as.data.frame(pam_res$medoids)
  centers$Cluster <- paste0("Cluster", 1:nrow(centers))
  df_long <- tidyr::pivot_longer(centers, -Cluster)

  ggplot(df_long, aes(x = name, y = value, group = Cluster, color = Cluster)) +
    geom_line() +
    geom_point() +
    coord_polar() +
    theme_minimal() +
    labs(title = "PAM Cluster Center Radar Plot")
}
