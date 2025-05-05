#' @title Summarise Coefficient of Variation
#'
#' @description
#' Calculates the coefficient of variation (CV = sd / mean) for numeric vectors, matrices, data frames, or tibbles.
#'
#' @param x A numeric vector, matrix, data frame, or tibble.
#'
#' @return A tibble:
#' - If input has one numeric column or is a numeric vector: a tibble with a single value.
#' - If input has multiple numeric columns: a tibble with variable names and coefficient of variation values.
#'
#' @examples
#' summarise_coef_of_variation(iris)
#' summarise_coef_of_variation(iris$Petal.Length)
#' summarise_coef_of_variation(data.frame(a = rnorm(100), b = runif(100)))
#' @rdname summarise_coef_of_variation
#' @export
summarise_coef_of_variation <- function(x) {
  # Coerce input to tibble
  if (is.vector(x) && is.numeric(x)) {
    x <- tibble::tibble(value = x)
  } else if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  
  x <- tibble::as_tibble(x)
  x <- dplyr::select(x, dplyr::where(is.numeric))
  
  if (ncol(x) == 0) stop("No numeric columns to compute coefficient of variation.")
  
  # --- Coefficient of Variation function ---
  cv_fn <- function(vec) {
    vec <- vec[!is.na(vec)]
    m <- mean(vec)
    s <- stats::sd(vec)
    if (m == 0) return(NA_real_)
    s / m
  }
  
  result <- vapply(x, cv_fn, numeric(1))
  
  # Return tidy tibble
  if (length(result) == 1) {
    return(tibble::tibble(coefficient_of_variation = result[[1]]))
  } else {
    return(tibble::tibble(variable = names(result), coefficient_of_variation = unname(result)))
  }
}
