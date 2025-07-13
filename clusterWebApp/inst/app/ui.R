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

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Universal Clustering Demonstration Platform"),

  sidebarLayout(
    sidebarPanel(
      h4("Clustering Parameters"),
      numericInput("seed", "Random Seed:", value = 123, min = 1, step = 1),
      selectInput("dataset", "Select Dataset:",
                  choices = c("Select" = "", "iris", "USArrests", "mtcars", "CO2", "swiss", "Moons", "Upload CSV")),
      conditionalPanel(
        condition = "input.dataset == 'Upload CSV'",
        fileInput("file", "Upload CSV File:", accept = ".csv")
      ),
      uiOutput("var_selector"),
      selectInput("method", "Clustering Method:",
                  choices = c("Select" = "", "KMeans", "Hierarchical", "DBSCAN", "PAM", "GMM", "Spectral")),
      numericInput("clusters", "Number of Clusters (K):", value = 0, min = 2, max = 10),
      conditionalPanel(
        condition = "input.method == 'DBSCAN'",
        numericInput("eps", "DBSCAN: eps", value = 0.5, min = 0.1, step = 0.1),
        numericInput("minPts", "DBSCAN: minPts", value = 5, min = 1)
      ),
      radioButtons("dim_red", "Dimension Reduction:", choices = c("PCA", "t-SNE"), selected = "PCA"),
      actionButton("run", "Run Clustering", class = "btn-primary"),
      hr(),
      radioButtons("view_type", "Right Panel Display:",
                   choices = c("Course Guide" = "guide", "Method Intro" = "methods", "Data Summary" = "summary", "Visualization" = "viz", "Evaluation Metrics" = "metrics"))
    ),

    mainPanel(
      tags$style(".well { background-color: #f9f9f9; padding: 10px; border-radius: 10px; }"),

      conditionalPanel(
        condition = "input.view_type == 'guide'",
        h4("Clustering Algorithm Guide"),
        HTML('<p>This module helps you systematically understand the core concepts and application scenarios of clustering analysis.</p>
        <ul>
          <li><b>Clustering</b> is an unsupervised learning technique that divides data into groups (clusters) such that samples within a cluster are similar and different between clusters.</li>
          <li><b>Clustering vs Classification:</b> Clustering does not require labels, whereas classification learns from labeled data.</li>
          <li><b>Common Methods:</b> KMeans, DBSCAN, Spectral Clustering, etc.</li>
          <li><b>Evaluation Challenge:</b> Lack of labels requires metrics such as Silhouette Coefficient, CH Index, and DB Index.</li>
          <li><b>Applications:</b> Market segmentation, image analysis, gene typing, cell subtype identification, etc.</li>
        </ul>
        <img src="main.png" width="95%">')
      ),

      conditionalPanel(
        condition = "input.view_type == 'methods'",
        h4("Clustering Method Introduction"),
        htmlOutput("methodDetail")
      ),

      conditionalPanel(
        condition = "input.view_type == 'summary'",
        h4("Data Summary"),
        verbatimTextOutput("dataSummary"),
        div(class = "well", shinycssloaders::withSpinner(plotOutput("distPlot"), color = "#0dc5c1"))
      ),

      conditionalPanel(
        condition = "input.view_type == 'viz'",
        fluidRow(
          column(6, div(class = "well", shinycssloaders::withSpinner(plotOutput("clusterPlot"), color = "#0dc5c1"))),
          column(6, div(class = "well", uiOutput("methodSpecificPlot")))
        ),
        fluidRow(
          column(12, div(class = "well", shinycssloaders::withSpinner(plotOutput("boxPlot"), color = "#0dc5c1")))
        )
      ),

      conditionalPanel(
        condition = "input.view_type == 'metrics'",
        h4("Clustering Evaluation Metrics"),
        fluidRow(
          column(6, div(class = "well", shinycssloaders::withSpinner(plotOutput("silPlot"), color = "#0dc5c1"))),
          column(6, div(class = "well", verbatimTextOutput("silhouette")))
        ),
        br(),
        div(class = "well", p("Silhouette Coefficient reflects both cohesion and separation of clusters. It ranges from -1 to 1; higher values indicate better-defined clusters."))
      )
    )
  )
)
