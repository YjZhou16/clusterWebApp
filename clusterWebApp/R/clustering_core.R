#' Perform clustering analysis
#'
#' This function performs clustering on a numeric matrix using one of six common clustering methods:
#' KMeans, Hierarchical, DBSCAN, PAM, Gaussian Mixture Model (GMM), or Spectral Clustering.
#'
#' @param data A numeric matrix or data frame, typically standardized, to be clustered.
#' @param method A string indicating the clustering method to use. Options are: "KMeans", "Hierarchical", "DBSCAN", "PAM", "GMM", "Spectral".
#' @param k An integer specifying the number of clusters. Required for KMeans, Hierarchical, PAM, GMM, and Spectral.
#' @param eps A numeric value specifying the epsilon parameter for DBSCAN. Default is 0.5.
#' @param minPts An integer specifying the minimum number of points for DBSCAN. Default is 5.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{cluster}{A vector of cluster labels assigned to each observation.}
#'   \item{silhouette}{An object of class \code{silhouette} representing silhouette widths.}
#' }
#'
#' @examples
#' data(iris)
#' result <- run_clustering(scale(iris[, 1:4]), method = "KMeans", k = 3)
#' print(result$cluster)
#' if (interactive()) {
#'   plot(result$silhouette)
#' }

#'
#' @export
#' @importFrom Rtsne Rtsne
#' @importFrom dplyr select mutate across
#' @importFrom dbscan dbscan
#' @importFrom kernlab specc
#' @importFrom mclust Mclust
#' @importFrom stats dist hclust cutree
run_clustering <- function(data, method, k = 3, eps = 0.5, minPts = 5) {
  cl <- NULL
  sil <- NULL
  if (method == "KMeans") {
    km <- kmeans(data, centers = k)
    cl <- km$cluster
    sil <- silhouette(cl, dist(data))
  } else if (method == "Hierarchical") {
    hc <- hclust(dist(data))
    cl <- cutree(hc, k = k)
    sil <- silhouette(cl, dist(data))
  } else if (method == "DBSCAN") {
    db <- dbscan(data, eps = eps, minPts = minPts)
    cl <- db$cluster
    valid <- cl != 0
    if (sum(valid) >= 2) {
      sil <- silhouette(cl[valid], dist(data[valid, , drop = FALSE]))
    }
  } else if (method == "PAM") {
    pam_res <- pam(data, k = k)
    cl <- pam_res$clustering
    sil <- silhouette(cl, dist(data))
  } else if (method == "GMM") {
    gmm <- Mclust(data, G = k)
    cl <- gmm$classification
    sil <- silhouette(cl, dist(data))
  } else if (method == "Spectral") {
    sp <- specc(data, centers = k)
    cl <- as.integer(sp)
    sil <- silhouette(cl, dist(data))
  }
  list(cluster = cl, silhouette = sil)
}

