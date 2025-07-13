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

prepare_data <- function(dataset) {
  switch(dataset,
         "iris" = iris[, -5],
         "USArrests" = na.omit(USArrests),
         "mtcars" = mtcars,
         "CO2" = CO2[, sapply(CO2, is.numeric)] %>% drop_na(),
         "swiss" = swiss,
         "Moons" = make_moons(n = 200, noise = 0.05))
}



ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("通用聚类模型演示平台"),

  sidebarLayout(
    sidebarPanel(
      h4("聚类参数设置"),
      numericInput("seed", "随机数种子：", value = 123, min = 1, step = 1),
      selectInput("dataset", "选择数据集：",
                  choices = c("请选择" = "", "iris", "USArrests", "mtcars", "CO2", "swiss", "Moons", "自定义上传")),
      conditionalPanel(
        condition = "input.dataset == '自定义上传'",
        fileInput("file", "上传CSV文件：", accept = ".csv")
      ),
      uiOutput("var_selector"),
      selectInput("method", "选择聚类方法：",
                  choices = c("请选择" = "", "KMeans", "Hierarchical", "DBSCAN", "PAM", "GMM", "Spectral")),
      numericInput("clusters", "簇数 (K)：", value = 0, min = 2, max = 10),
      conditionalPanel(
        condition = "input.method == 'DBSCAN'",
        numericInput("eps", "DBSCAN eps 值：", value = 0.5, min = 0.1, step = 0.1),
        numericInput("minPts", "DBSCAN minPts 值：", value = 5, min = 1)
      ),
      radioButtons("dim_red", "降维方式：", choices = c("PCA", "t-SNE"), selected = "PCA"),
      actionButton("run", "开始聚类", class = "btn-primary"),
      hr(),
      radioButtons("view_type", "右侧内容显示：",
                   choices = c("课程导览" = "guide", "方法简介" = "methods", "数据摘要" = "summary", "可视化" = "viz", "评估指标" = "metrics"))
    ),

    mainPanel(
      tags$style(".well { background-color: #f9f9f9; padding: 10px; border-radius: 10px; }"),
      conditionalPanel(
        condition = "input.view_type == 'guide'",
        h4("聚类算法课程导览"),
        HTML('<p>本模块旨在帮助你系统性地理解聚类分析的核心概念与应用场景。</p>
        <ul>
          <li><b>聚类</b>属于无监督学习范畴，用于将数据集划分为若干簇（Cluster），使同一簇内样本相似度高，不同簇之间差异大。</li>
          <li><b>聚类 vs 分类</b>：聚类无需标签，分类则基于已有标签学习分类规则。</li>
          <li><b>常见聚类方法</b>包括基于划分的KMeans，基于密度的DBSCAN，基于图结构的谱聚类等。</li>
          <li><b>聚类评估难点</b>在于标签未知，需借助轮廓系数、CH指数、DB指数等指标。</li>
          <li><b>应用案例</b>：市场细分、图像处理、基因分型、细胞亚型识别等。</li>
        </ul>
        <img src="main.png" width="95%">')
      ),

      conditionalPanel(
        condition = "input.view_type == 'methods'",
        h4("聚类方法简介"),
        htmlOutput("methodDetail")
      ),

      conditionalPanel(
        condition = "input.view_type == 'summary'",
        h4("数据摘要"),
        verbatimTextOutput("dataSummary"),
        div(class = "well", plotOutput("distPlot") %>% shinycssloaders::withSpinner(color="#0dc5c1"))
      ),

      conditionalPanel(
        condition = "input.view_type == 'viz'",
        fluidRow(
          column(6, div(class = "well", plotOutput("clusterPlot") %>% shinycssloaders::withSpinner(color="#0dc5c1"))),
          column(6, div(class = "well", uiOutput("methodSpecificPlot")))
        ),
        fluidRow(
          column(12, div(class = "well", plotOutput("boxPlot") %>% shinycssloaders::withSpinner(color="#0dc5c1")))
        )
      ),

      conditionalPanel(
        condition = "input.view_type == 'metrics'",
        h4("聚类评估指标"),
        fluidRow(
          column(6, div(class = "well", plotOutput("silPlot") %>% shinycssloaders::withSpinner(color="#0dc5c1"))),
          column(6, div(class = "well", verbatimTextOutput("silhouette")))
        ),
        br(),
        div(class = "well", p("轮廓系数（Silhouette Coefficient）反映聚类效果的紧密度与分离度。其值介于 -1 和 1 之间，值越大表示聚类结果越合理。"))
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data_reactive <- reactive({
    if (input$dataset == "Moons") {
      moons <- mlbench::mlbench.smiley(200, sd1 = 0.05)
      return(as.data.frame(moons$x))
    }
    req(input$dataset != "")
    if (input$dataset == "自定义上传") {
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
    checkboxGroupInput("features", "选择纳入聚类的变量：",
                       choices = names(df), selected = names(df))
  })

  observeEvent(input$run, {
    set.seed(input$seed)
    if (input$dataset == "") {
    showModal(modalDialog(title = "错误", "请先选择数据！", easyClose = TRUE))
    return()
  }
  if (input$method == "") {
    showModal(modalDialog(title = "错误", "请先选择聚类方法！", easyClose = TRUE))
    return()
  }
  if (input$clusters <= 0 && input$method != "DBSCAN") {
    showModal(modalDialog(title = "错误", "簇数必须大于0！", easyClose = TRUE))
    return()
  }
    req(input$dataset != "", input$method != "", input$features)

    raw_data <- raw_data_reactive()
    pre_data <- raw_data %>% select(all_of(input$features)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    data <- scale(pre_data)

    # 降维
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
    labs(title = paste("聚类结果 -", input$method), x = "维度1", y = "维度2")

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
    labs(title = "肘部法", x = "簇数", y = "总平方和")
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
    labs(title = "簇中心雷达图")
})

output$emptyPlot <- renderPlot({
  plot.new(); text(0.5, 0.5, "此方法暂无附加图展示", cex = 1.2)
})

    output$silPlot <- renderPlot({
      if (!is.null(sil)) {
        fviz_silhouette(sil)
      } else {
        plot.new(); text(0.5, 0.5, "轮廓图不可用")
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
    labs(title = "特征分布箱线图（按簇分组）")
})

output$silhouette <- renderPrint({
      if (!is.null(sil)) {
        mean(sil[, 3])
      } else {
        "无法计算轮廓系数。"
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
      HTML("<h4>KMeans 聚类</h4>
        <p>KMeans 是一种基于划分的迭代聚类方法，适合处理数值型特征数据。其核心思想是通过最小化类内平方误差来不断优化簇划分。</p>
        <ol>
          <li>从样本中随机选择 K 个初始聚类中心</li>
          <li>根据欧氏距离将每个样本分配到最近的中心所属簇</li>
          <li>重新计算每个簇中所有样本的均值作为新中心</li>
          <li>重复步骤 2-3，直到簇分配不再发生变化或达到最大迭代次数</li>
        </ol>
        <img src='kmeans_steps.png' width='80%'>")
    } else if (input$method == "DBSCAN") {
      HTML("<h4>DBSCAN 聚类</h4>
        <p>DBSCAN（Density-Based Spatial Clustering of Applications with Noise）是一种基于密度的聚类算法，适用于检测任意形状的簇和识别噪声点。</p>
        <ol>
          <li>定义两个参数：eps（邻域半径）和 minPts（最小邻域点数）</li>
          <li>将密度高的区域扩展为簇，形成密度可达的点集</li>
          <li>未能归入任何簇的点视为噪声</li>
        </ol>
        <img src='dbscan_steps.png' width='80%'>")
    } else if (input$method == "Hierarchical") {
      HTML("<h4>层次聚类</h4>
        <p>层次聚类构建的是样本之间的嵌套层级结构，通过逐步合并或分裂样本簇来形成聚类树（dendrogram）。</p>
        <ol>
          <li>每个样本初始为单独一类</li>
          <li>计算所有样本对之间的距离，合并最近的一对簇</li>
          <li>更新距离矩阵，继续合并，直到所有样本合并为一个簇</li>
          <li>通过截断聚类树得到所需簇数</li>
        </ol>
        <img src='hierarchical_steps.png' width='80%'>")
    } else if (input$method == "PAM") {
      HTML("<h4>PAM 聚类（Partitioning Around Medoids）</h4>
        <p>PAM 是一种基于中心对象的聚类算法，使用样本点作为中心（medoid），比 KMeans 对异常值更鲁棒。</p>
        <ol>
          <li>从数据中选择 K 个代表点作为初始 medoids</li>
          <li>将每个样本分配到最近的 medoid 所代表的簇</li>
          <li>尝试用非中心点替换 medoid，若成本降低则更新</li>
          <li>重复直到所有 medoid 不再更新</li>
        </ol>
        <img src='pam_steps.png' width='80%'>")
    } else if (input$method == "GMM") {
      HTML("<h4>GMM 聚类（Gaussian Mixture Model）</h4>
        <p>高斯混合模型是一种基于概率的软聚类方法，通过最大化样本来自多个高斯分布的联合概率来拟合聚类。</p>
        <ol>
          <li>初始化各高斯分布的参数（均值、协方差和权重）</li>
          <li>使用期望最大化（EM）算法交替更新分布参数</li>
          <li>每个样本根据属于各分布的概率进行软分配</li>
          <li>训练收敛后，根据最大后验概率决定最终簇</li>
        </ol>
        <img src='gmm_steps.png' width='80%'>")
    } else if (input$method == "Spectral") {
      HTML("<h4>谱聚类（Spectral Clustering）</h4>
        <p>谱聚类基于图论思想，通过构建相似度图并提取其拉普拉斯矩阵的特征向量，对高维或非凸数据具有良好的聚类效果。</p>
        <ol>
          <li>构建样本之间的相似度矩阵（如高斯核）</li>
          <li>计算图的拉普拉斯矩阵</li>
          <li>提取前 K 个特征向量构成嵌入空间</li>
          <li>在该空间中使用 KMeans 进行聚类</li>
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
      labs(title = "变量分布图")
  })
}

shinyApp(ui, server)



