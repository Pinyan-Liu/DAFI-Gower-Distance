# DAFI-Gower Distance Algorithm

## Overview

The **DAFI-Gower** algorithm is a modified Gower distance-based clustering technique designed to handle **mixed-type data**, which includes both continuous and categorical variables. This method introduces the concept of **feature importance as weights**, enhancing clustering performance by ensuring that both feature types contribute equally to the clustering process. 

The algorithm has been successfully evaluated on **simulated datasets** and **real-world clinical data** from the **2011-2014 National Health and Nutrition Examination Survey (NHANES)**. The DAFI-Gower algorithm provides **better clustering compatibility, adaptability, and interpretability** compared to traditional mixed-type clustering techniques.

## Features

- **Mixed-Type Data Handling**: Effectively clusters datasets containing both continuous and categorical variables.
- **Feature Importance Weighting**: Incorporates feature importance to balance the contributions of different feature types.
- **Optimized Performance**: Consistently outperforms baseline clustering methods in simulation studies, particularly on datasets with redundant features.
- **Real-World Applicability**: Demonstrates high clustering performance on NHANES datasets with distinct health profiles.

## Methodology

### Gower Distance with Feature Importance
DAFI-Gower is a **modified Gower distance** that incorporates feature importance weights to address the challenge of combining categorical and continuous variables in clustering. The feature importance weights ensure that both types of features contribute equally to cluster formation, which is especially important when working with clinical datasets where some variables might be irrelevant or redundant.

### Evaluation Metrics
- **Adjusted Rand Index (ARI)**: Used for assessing clustering accuracy in simulated studies.
- **Silhouette Score**: Used to evaluate cohesion and separation in the NHANES dataset.
- **Logistic Regression**: Used to estimate associations between clusters and real-world clinical outcomes, such as the relationship between periodontitis and cardiovascular diseases.

## Results

- **Simulated Data**: The DAFI-Gower algorithm outperforms 13 baseline clustering methods according to the Adjusted Rand Index, especially in datasets with redundant features.
- **NHANES Data**: 
  - Achieved the highest silhouette score.
  - Identified **four distinct clusters** with diverse health profiles.
  - Showed that cluster formations were more influenced by **cardiovascular disease (CVD)**-related factors.
  
### Clinical Insights
A multivariable logistic regression analysis revealed a significant association between periodontitis (PD) and cardiovascular diseases (CVDs), adjusting for clusters. The findings suggest that **severe periodontitis** is a potential **risk factor for cardiovascular diseases** (adjusted OR 1.95, 95% CI 1.50 to 2.55, p = 0.012).Moreover, utilizing clusters in association analyses offers **distinct methodological and clinical advantages**.

## Conclusion

The **DAFI-Gower** algorithm offers a robust, interpretable, and effective solution for clustering **mixed-type data**, particularly in clinical datasets. By incorporating **feature importance**, DAFI ensures that the clustering process accounts for the real-world significance of features, leading to **better clustering accuracy** and more **meaningful health insights**.

## Getting Started

### Prerequisites
To use the DAFI-Gower algorithm, you will need to source the following functions:
- `self_adaptive_distance.R`
- `gower.dist.modify.R`
- `normalized_MI.R`

### Source
To source the required dependencies, run:

```bash
source(".../self_adaptive_distance.R")
source(".../gower.dist.modify.R")
source(".../normalized_MI.R")
```

To perform the data analysis, run:

```bash
# Get feature importance
  feature_importance1 = self_adaptive_distance(continuous_feature, categorical_feature)
  feature_importance2 = normalized_MI(continuous_feature, categorical_feature)
  feature_importance <- feature_importance1*feature_importance2
  feature_importance <- feature_importance/sum(feature_importance)
  
  dist_matrix <- gower.dist.modify2(data, var.weights = feature_importance, robcb = "iqr")
```
