#' @title Summarise Boxplot Statistics with Outliers
#'
#' @description
#' Computes the five-number summary (min, Q1, median, Q3, max), interquartile range (IQR), range, and 
#' outliers for each numeric variable in a data frame or a numeric vector.
#'
#' @param x A numeric vector, matrix, data frame, or tibble.
#'
#' @return A tibble with columns: `variable`, `min`, `q1`, `median`, `q3`, `max`, `iqr`, `range`, `n_outliers`, `outliers`.
#'
#' @examples
#' summarise_boxplot_stats(iris)
#' summarise_boxplot_stats(iris$Sepal.Width)
#' summarise_boxplot_stats(data.frame(a = c(rnorm(98), 10, NA)))
#'
#' @rdname summarise_boxplot_stats
#' @export
summarise_boxplot_stats <- function(x) {
  # Coerce to tibble and select numeric columns
  if (is.vector(x) && is.numeric(x)) {
    x <- tibble::tibble(value = x)
  } else if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  
  x <- tibble::as_tibble(x)
  x <- dplyr::select(x, dplyr::where(is.numeric))
  
  if (ncol(x) == 0) {
    stop("No numeric columns found.")
  }
  
  # Helper function
  box_stats <- function(vec) {
    vec <- stats::na.omit(vec)
    q <- stats::quantile(vec, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
    names(q) <- c("min", "q1", "median", "q3", "max")
    iqr <- q["q3"] - q["q1"]
    rng <- q["max"] - q["min"]
    
    # Outlier thresholds
    lower <- q["q1"] - 1.5 * iqr
    upper <- q["q3"] + 1.5 * iqr
    outliers <- vec[vec < lower | vec > upper]
    
    tibble::tibble(
      min = q["min"],
      q1 = q["q1"],
      median = q["median"],
      q3 = q["q3"],
      max = q["max"],
      iqr = iqr,
      range = rng,
      n_outliers = length(outliers),
      outliers = list(outliers)
    )
  }
  
  result <- purrr::imap_dfr(
    x,
    ~ dplyr::mutate(box_stats(.x), variable = .y),
    .id = NULL
  ) %>%
    dplyr::relocate(variable, .before = 1)
  
  return(result)
}
