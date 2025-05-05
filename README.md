
# tidySummaries <img src="man/figures/logo.png" align="right" width="150" alt="tidySummaries logo" />

![tidyverse-friendly](https://img.shields.io/badge/tidyverse-friendly-blueviolet)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/tidySummaries)](https://cran.r-project.org/package=tidySummaries)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidySummaries)](https://CRAN.R-project.org/package=tidySummaries)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)

**Tidy statistical summaries made simple**

`tidySummaries` provides a modern and extensible set of functions for
descriptive statistics, frequency analysis, and significance testing —
all with **tidy output**.

It’s ideal for both **numeric** and **categorical** exploratory data
analysis, supporting group comparisons, normality checks, console
coloring, and more.

------------------------------------------------------------------------

## Features

- **Tidy descriptive statistics**  
  `summarise_statistics()` computes mean, median, standard deviation,
  variance, skewness, kurtosis, IQR, MAD, and CV in a single tidy
  tibble.

- **Frequency tables**  
  `summarise_frequency()` summarizes categorical variables with
  frequency counts, proportions, or percentages.

- **Normality and group significance testing**  
  Automatically perform Shapiro-Wilk tests for normality, plus t-tests,
  Wilcoxon tests, ANOVA, or Kruskal-Wallis tests for group comparisons.

- **Grouped summaries**  
  `summarise_group_stats()` groups data by one or more variables and
  summarizes selected numeric columns flexibly.

- **Correlation analysis**  
  `summarise_correlation()` computes pairwise correlations (Pearson,
  Spearman, Kendall) and highlights significant results.

- **Boxplot statistics with outlier detection**  
  `summarise_boxplot_stats()` returns min, Q1, median, Q3, max, range,
  IQR, and detected outliers for numeric data.

- **Colored console output for significance**  
  Statistically significant results are automatically highlighted in red
  for easy identification.

- **Support for vectors, matrices, and data frames**  
  Functions handle vectors, matrices, tibbles, and grouped data frames
  smoothly.

- **Tidyverse-friendly design** Pipeable and fully compatible with
  tidyverse workflows. All outputs are clean tibbles ready for further
  analysis or visualization.

------------------------------------------------------------------------

## Installation

You can install the development version from GitHub:

\`\`\`r \# install.packages(“devtools”)
devtools::install_github(“kleanthisk10/tidySummaries”)
