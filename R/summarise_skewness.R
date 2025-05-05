#' @title Summarise Skewness
#'
#' @description
#' Calculates skewness for numeric vectors, matrices, data frames, or tibbles using Pearson’s moment coefficient.
#'
#' @param x A numeric vector, matrix, data frame, or tibble.
#'
#' @return A tibble:
#' - If input has one numeric column or is a numeric vector: a tibble with a single value.
#' - If input has multiple numeric columns: a tibble with variable names and skewness values.
#'
#' @examples
#' summarise_skewness(iris)
#' summarise_skewness(as.vector(iris$Sepal.Width))
#' summarise_skewness(data.frame(a = rnorm(100), b = rgamma(100, 2)))
#' @rdname summarise_skewness
#' @export
summarise_skewness <- function(x) {
  # Coerce input to tibble
  if (is.vector(x) && is.numeric(x)) {
    x <- tibble::tibble(value = x)
  } else if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  
  x <- tibble::as_tibble(x)
  x <- dplyr::select(x, dplyr::where(is.numeric))
  
  if (ncol(x) == 0) stop("No numeric columns to compute skewness.")
  
  # --- Skewness function ---
  skewness_fn <- function(vec) {
    vec <- vec[!is.na(vec)]
    n <- length(vec)
    if (n < 1) return(NA_real_)
    m <- mean(vec)
    s <- sd(vec)
    if (s == 0) return(NA_real_)
    m3 <- mean((vec - m)^3)
    m3 / (s^3)
  }
  
  result <- vapply(x, skewness_fn, numeric(1))
  
  # Return tidy tibble
  if (length(result) == 1) {
    return(tibble::tibble(skewness = result[[1]]))
  } else {
    return(tibble::tibble(variable = names(result), skewness = unname(result)))
  }
}
