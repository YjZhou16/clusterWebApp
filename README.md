<div align=center>
<img src='./inst/app/www/main.png' width="50%" >
</div>

##

[![GitHub stars](https://img.shields.io/github/stars/YjZhou16/clusterWebApp?style=flat\&logo=github\&color=orange)](https://github.com/YjZhou16/clusterWebApp/stargazers)
[![Last commit](https://img.shields.io/github/last-commit/YjZhou16/clusterWebApp?logo=git)](https://github.com/YjZhou16/clusterWebApp/commits/main)

<!--
[![GitHub forks](https://img.shields.io/github/forks/YjZhou16/clusterWebApp?style=flat&logo=github)](https://github.com/YjZhou16/clusterWebApp/network)
[![GitHub issues](https://img.shields.io/github/issues/YjZhou16/clusterWebApp?style=flat)](https://github.com/YjZhou16/clusterWebApp/issues)
[![GitHub license](https://img.shields.io/github/license/YjZhou16/clusterWebApp)](https://github.com/YjZhou16/clusterWebApp/blob/main/LICENSE)
-->

## **ClusterWebApp**: Interactive Clustering Analysis Platform

**ClusterWebApp** is a lightweight and user-friendly R Shiny application that integrates multiple popular clustering algorithms with an interactive interface. It’s tailored for both educational purposes and practical data analysis, offering insightful visualizations and performance evaluations.

<div align=center>
<img src='./inst/app/www/kmeans_steps.png' width="100%" >
</div>

## Installation

```r
# install.packages("devtools")
devtools::install_github("YjZhou16/clusterWebApp")
```

Alternatively, clone this repository and run locally:

```r
git clone https://github.com/YjZhou16/clusterWebApp.git
setwd("clusterWebApp")
shiny::runApp("inst/app")
```

## Key Features of ClusterWebApp

* Support for **built-in datasets** (e.g., `iris`, `mtcars`) and **custom CSV uploads**
* Integration of **6 major clustering methods**:

  * K-Means
  * PAM (Partitioning Around Medoids)
  * Spectral Clustering
  * DBSCAN
  * Gaussian Mixture Models (GMM)
  * Hierarchical Clustering
* Automatic numeric variable filtering and missing value handling
* Dimensionality reduction via **PCA** or **t-SNE**
* Auto-generated:

  * Clustering result plots
  * Silhouette plots
  * Radar charts
  * Boxplots
* Clustering quality evaluated by:

  * **Silhouette Coefficient**
  * **Elbow Method**

## Project Structure

```
clusterWebApp/
├── inst/app/
│   ├── ui.R
│   ├── server.R
│   └── www/
│       ├── *.png  (algorithm steps images)
├── R/
│   ├── run_app.R
│   ├── prepare_data.R
│   ├── clustering_core.R
│   └── plotting and metric functions
├── man/               # documentation
├── DESCRIPTION        # package metadata
├── NAMESPACE
└── LICENSE
```

## Quick Start

0. Open app.R and `Run App` or follow the steps below:
1. Launch the app with `run_app()`
2. Set a random seed (default: `123`)
3. Choose a dataset (built-in or uploaded)
4. Select a clustering method (KMeans, DBSCAN, etc.)
5. Configure method-specific parameters (e.g., `k`, `eps`, `minPts`)
6. Run clustering and explore the visualized results in real-time

## Algorithm Summary

| Method           | Description                         | Advantages                         | Use Cases                |
| ---------------- | ----------------------------------- | ---------------------------------- | ------------------------ |
| **K-Means**      | Iterative centroid-based clustering | Fast, scalable                     | Spherical clusters       |
| **Hierarchical** | Tree-based merging                  | Interpretable dendrograms          | Small datasets           |
| **DBSCAN**       | Density-based with noise detection  | Handles noise and arbitrary shapes | Spatial or noisy data    |
| **PAM**          | Medoid-based clustering             | Robust to outliers                 | Irregular distributions  |
| **GMM**          | Probabilistic soft clustering       | Mixture modeling                   | Multimodal distributions |
| **Spectral**     | Graph Laplacian-based method        | Captures non-convex structures     | Complex manifolds        |

## 📢 News

- *July 13th, 2025*: Initial release of the `clusterWebApp` package with 6 clustering algorithms and full Shiny UI support.

## 🤝 Contributing

Contributions, feedback, and feature requests are welcome! Please reach out via GitHub Issues or contact **Yijin Zhou** at [yijin\_zhou1116@163.com](mailto:yijin_zhou1116@163.com).

---



