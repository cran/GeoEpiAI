# GeoEpiAI 0.1.0

GeoEpiAI provides an initial R toolkit for GeoAI and digital epidemiology,
with functions for surveillance-data cleaning, spatial epidemiology,
environmental-data integration, machine-learning-based disease-risk
prediction, early-warning detection, dynamic risk mapping, and automated
epidemiological reporting.

## Features

1. Surveillance-data cleaning and validation
2. Global Moran's I and local Getis-Ord Gi* hotspot detection
3. Random Forest and XGBoost binary disease-risk prediction
4. Tabular and spatial environmental-data integration
5. Dynamic/faceted disease-risk maps
6. Rolling-baseline early-warning detection
7. Automated HTML epidemiological reports

## Installation

Install the required dependencies:

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "lubridate",
  "randomForest",
  "sf",
  "spdep",
  "xgboost"
))
