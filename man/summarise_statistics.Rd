% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_statistics.R
\name{summarise_statistics}
\alias{summarise_statistics}
\title{Summarise Descriptive Statistics with Optional Testing}
\usage{
summarise_statistics(
  data,
  group_var = NULL,
  normality_test = FALSE,
  group_test = FALSE,
  show_colors = TRUE
)
}
\arguments{
\item{data}{A numeric vector, matrix, or data frame.}

\item{group_var}{Optional. A character name of a grouping variable.}

\item{normality_test}{Logical. If TRUE, performs Shapiro-Wilk test for normality.}

\item{group_test}{Logical. If TRUE and `group_var` is set, performs group-wise significance tests (t-test, ANOVA, etc.).}

\item{show_colors}{Logical. If TRUE and `group_test` is TRUE, prints colored console output for significant group results. Default is TRUE.}
}
\value{
A tibble with descriptive statistics and optional test results per numeric variable.
}
\description{
Computes descriptive statistics for numeric data. Optionally groups by a variable and includes Shapiro-Wilk and group significance testing. Can color console output for significant differences.
}
\examples{
summarise_statistics(iris, group_var = "Species", group_test = TRUE)

}
