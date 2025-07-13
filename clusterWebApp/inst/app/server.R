#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny numericInput selectInput fileInput radioButtons actionButton conditionalPanel uiOutput verbatimTextOutput plotOutput column h4
#' @importFrom shinythemes shinytheme
#' @importFrom shinycssloaders withSpinner
#' @importFrom magrittr %>%
#' @importFrom stats na.omit dist hclust cutree
library(shiny)
library(cluster)
library(factoextra)
library(datasets)
library(ggplot2)
library(dbscan)
library(mclust)
library(kernlab)
library(Rtsne)
library(DT)
library(dplyr)
library(tidyr)
library(mlbench)
library(shinythemes)


server <- function(input, output, session) {
  raw_data_reactive <- reactive({
    if (input$dataset == "Moons") {
      moons <- mlbench::mlbench.smiley(200, sd1 = 0.05)
      return(as.data.frame(moons$x))
    }
    req(input$dataset != "")
    if (input$dataset == "Upload CSV") {
      req(input$file)
      df <- read.csv(input$file$datapath, header = TRUE)
      df <- df[, sapply(df, is.numeric), drop = FALSE]
      return(na.omit(df))
    } else {
      prepare_data(input$dataset)
    }
  })

  output$var_selector <- renderUI({
    df <- raw_data_reactive()
    checkboxGroupInput("features", "Variables for Clustering:",
                       choices = names(df), selected = names(df))
  })

  observeEvent(input$run, {
    set.seed(input$seed)
    if (input$dataset == "") {
      showModal(modalDialog(title = "Error", "Please select a dataset.", easyClose = TRUE))
      return()
    }
    if (input$method == "") {
      showModal(modalDialog(title = "Error", "Please select a clustering method.", easyClose = TRUE))
      return()
    }
    if (input$clusters <= 0 && input$method != "DBSCAN") {
      showModal(modalDialog(title = "Error", "Cluster number must be greater than 0.", easyClose = TRUE))
      return()
    }
    req(input$dataset != "", input$method != "", input$features)

    raw_data <- raw_data_reactive()
    pre_data <- raw_data %>% select(all_of(input$features)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    data <- scale(pre_data)

    dim_data <- if (input$dim_red == "PCA") {
      prcomp(data)$x[, 1:2]
    } else {
      Rtsne(data, dims = 2, perplexity = 10, check_duplicates = FALSE)$Y
    }
    colnames(dim_data) <- c("Dim1", "Dim2")

    cl <- NULL
    sil <- NULL

    if (input$method == "KMeans") {
      km <- kmeans(data, centers = input$clusters)
      cl <- km$cluster
      sil <- silhouette(cl, dist(data))
    } else if (input$method == "Hierarchical") {
      hc <- hclust(dist(data))
      cl <- cutree(hc, k = input$clusters)
      sil <- silhouette(cl, dist(data))
    } else if (input$method == "DBSCAN") {
      db <- dbscan(data, eps = input$eps, minPts = input$minPts)
      cl <- db$cluster
      valid <- cl != 0
      if (sum(valid) >= 2) {
        sil <- silhouette(cl[valid], dist(data[valid, , drop = FALSE]))
      }
    } else if (input$method == "PAM") {
      pam_res <- pam(data, k = input$clusters)
      cl <- pam_res$clustering
      sil <- silhouette(cl, dist(data))
    } else if (input$method == "GMM") {
      gmm <- Mclust(data, G = input$clusters)
      cl <- gmm$classification
      sil <- silhouette(cl, dist(data))
    } else if (input$method == "Spectral") {
      sp <- specc(data, centers = input$clusters)
      cl <- as.integer(sp)
      sil <- silhouette(cl, dist(data))
    }

    output$clusterPlot <- renderPlot({
      df <- as.data.frame(dim_data)
      df$cluster <- factor(cl)

      p <- ggplot(df, aes(x = Dim1, y = Dim2, color = cluster, fill = cluster)) +
        stat_ellipse(geom = "polygon", alpha = 0.2, color = NA) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = paste("Clustering Result -", input$method), x = "Dimension1", y = "Dimension2")

      if (!is.null(sil)) {
        cluster_centers <- aggregate(df[, 1:2], by = list(cluster = df$cluster), FUN = mean)
        p <- p + geom_text(data = cluster_centers, aes(x = Dim1, y = Dim2, label = cluster), size = 5, fontface = "bold", color = "black")
      }

      return(p)
    })

    output$methodSpecificPlot <- renderUI({
      if (input$method == "KMeans") {
        plotOutput("elbowPlot")
      } else if (input$method == "PAM") {
        plotOutput("radarPlot")
      } else {
        plotOutput("emptyPlot")
      }
    })

    output$elbowPlot <- renderPlot({
      req(input$method == "KMeans")
      wss <- sapply(1:10, function(k) kmeans(data, centers = k)$tot.withinss)
      df <- data.frame(k = 1:10, wss = wss)
      ggplot(df, aes(k, wss)) +
        geom_line() + geom_point() +
        labs(title = "Elbow Method", x = "Number of Clusters", y = "Total Within-Cluster SS")
    })

    output$radarPlot <- renderPlot({
      req(input$method == "PAM")
      df <- as.data.frame(data)
      pam_res <- pam(df, k = input$clusters)
      centers <- as.data.frame(pam_res$medoids)
      centers$Cluster <- paste0("Cluster", 1:nrow(centers))
      df_long <- pivot_longer(centers, -Cluster)
      ggplot(df_long, aes(x = name, y = value, group = Cluster, color = Cluster)) +
        geom_line() + geom_point() +
        coord_polar() +
        theme_minimal() +
        labs(title = "Cluster Centers Radar Plot")
    })

    output$emptyPlot <- renderPlot({
      plot.new(); text(0.5, 0.5, "No additional plot available for this method.", cex = 1.2)
    })

    output$silPlot <- renderPlot({
      if (!is.null(sil)) {
        fviz_silhouette(sil)
      } else {
        plot.new(); text(0.5, 0.5, "Silhouette plot not available.")
      }
    })



    output$boxPlot <- renderPlot({
      req(cl)
      df <- as.data.frame(data)
      df$Cluster <- factor(cl)
      df_long <- pivot_longer(df, -Cluster)
      ggplot(df_long, aes(x = Cluster, y = value, fill = Cluster)) +
        geom_boxplot() +
        facet_wrap(~name, scales = "free") +
        theme_minimal() +
        labs(title = "Feature Distribution by Cluster")
    })

    output$silhouette <- renderPrint({
      if (!is.null(sil)) {
        mean(sil[, 3])
      } else {
        "Silhouette score not available."
      }
    })
    updateRadioButtons(session, "view_type", selected = "viz")
  })

  observeEvent(input$dataset, {
    if (input$dataset != "") {
      updateRadioButtons(session, "view_type", selected = "summary")
    }
  })

  output$methodDetail <- renderUI({
    if (input$method == "KMeans") {
      HTML("<h4>KMeans Clustering</h4>
      <p>KMeans is a partition-based iterative clustering method suitable for numerical feature data. Its core idea is to optimize cluster assignments by minimizing within-cluster sum of squares (WCSS).</p>
      <ol>
        <li>Randomly select K initial cluster centers from the dataset</li>
        <li>Assign each sample to the nearest cluster based on Euclidean distance</li>
        <li>Recalculate the mean of all samples in each cluster as the new center</li>
        <li>Repeat steps 2–3 until assignments stabilize or the max iteration limit is reached</li>
      </ol>
      <img src='kmeans_steps.png' width='80%'>")
    } else if (input$method == "DBSCAN") {
      HTML("<h4>DBSCAN Clustering</h4>
      <p>DBSCAN (Density-Based Spatial Clustering of Applications with Noise) is a density-based algorithm that can detect clusters of arbitrary shape and identify noise points.</p>
      <ol>
        <li>Define two parameters: eps (neighborhood radius) and minPts (minimum points in neighborhood)</li>
        <li>Expand dense regions into clusters based on density reachability</li>
        <li>Points not belonging to any cluster are treated as noise</li>
      </ol>
      <img src='dbscan_steps.png' width='80%'>")
    } else if (input$method == "Hierarchical") {
      HTML("<h4>Hierarchical Clustering</h4>
      <p>Hierarchical clustering builds a nested structure among samples, forming a dendrogram by iteratively merging or splitting clusters.</p>
      <ol>
        <li>Initialize each sample as its own cluster</li>
        <li>Compute pairwise distances and merge the closest two clusters</li>
        <li>Update the distance matrix and continue merging</li>
        <li>Cut the dendrogram at the desired level to get the required number of clusters</li>
      </ol>
      <img src='hierarchical_steps.png' width='80%'>")
    } else if (input$method == "PAM") {
      HTML("<h4>PAM Clustering (Partitioning Around Medoids)</h4>
      <p>PAM is a medoid-based clustering algorithm that selects actual data points as centers, making it more robust to outliers than KMeans.</p>
      <ol>
        <li>Select K representative points (medoids) from the data as initial centers</li>
        <li>Assign each sample to the nearest medoid</li>
        <li>Try swapping medoids with non-medoids and update if total cost decreases</li>
        <li>Repeat until medoids no longer change</li>
      </ol>
      <img src='pam_steps.png' width='80%'>")
    } else if (input$method == "GMM") {
      HTML("<h4>GMM Clustering (Gaussian Mixture Model)</h4>
      <p>GMM is a soft clustering method based on probability. It fits clusters by maximizing the likelihood that data points are generated from multiple Gaussian distributions.</p>
      <ol>
        <li>Initialize the parameters (mean, covariance, and weight) of each Gaussian component</li>
        <li>Use the Expectation-Maximization (EM) algorithm to iteratively update parameters</li>
        <li>Assign samples to components probabilistically (soft assignment)</li>
        <li>After convergence, assign each sample to the component with highest posterior probability</li>
      </ol>
      <img src='gmm_steps.png' width='80%'>")
    } else if (input$method == "Spectral") {
      HTML("<h4>Spectral Clustering</h4>
      <p>Spectral clustering is based on graph theory. It constructs a similarity graph and extracts eigenvectors of its Laplacian matrix, making it effective for high-dimensional or non-convex data.</p>
      <ol>
        <li>Construct a similarity matrix (e.g., Gaussian kernel) between samples</li>
        <li>Compute the Laplacian matrix of the graph</li>
        <li>Extract the top K eigenvectors to form an embedding space</li>
        <li>Apply KMeans clustering in the new space</li>
      </ol>
      <img src='spectral_steps.png' width='80%'>")
    }
  })

  output$dataSummary <- renderPrint({
    req(input$dataset != "")
    summary(raw_data_reactive())
  })

  output$distPlot <- renderPlot({
    df <- raw_data_reactive()
    df_long <- pivot_longer(df, everything(), names_to = "Variable", values_to = "Value")
    ggplot(df_long, aes(x = Value)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      facet_wrap(~ Variable, scales = "free") +
      theme_minimal() +
      labs(title = "Feature Distributions")
  })
}
