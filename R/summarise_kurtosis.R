#' @title Summarise Kurtosis
#'
#' @description
#' Calculates the kurtosis (default: **excess kurtosis**) of numeric vectors, matrices, data frames, or tibbles.
#' Supports both the "standard" and "unbiased" methods and optionally returns **raw kurtosis**.
#'
#' @param x A numeric vector, matrix, data frame, or tibble.
#' @param method Character. Method for kurtosis calculation: `"standard"` (default) or `"unbiased"`.
#' @param excess Logical. If TRUE (default), returns **excess kurtosis** (minus 3); if FALSE, returns **raw kurtosis**.
#'
#' @return A tibble:
#' - If input has one numeric column (or is a vector), a single-row tibble.
#' - If input has multiple numeric columns, a tibble with variable names and kurtosis values.
#'
#' @examples
#' summarise_kurtosis(iris)
#' summarise_kurtosis(iris, method = "unbiased")
#' summarise_kurtosis(iris, excess = FALSE)  # Raw kurtosis
#' summarise_kurtosis(iris$Sepal.Width)
#'
#' @rdname summarise_kurtosis
#' @export
summarise_kurtosis <- function(x, method = c("standard", "unbiased"), excess = TRUE) {
  method <- match.arg(method)
  
  # Coerce input
  if (is.vector(x) && is.numeric(x)) {
    x <- tibble::tibble(value = x)
  } else if (is.matrix(x)) {
    x <- as.data.frame(x)
  }
  
  x <- tibble::as_tibble(x)
  x <- dplyr::select(x, dplyr::where(is.numeric))
  
  if (ncol(x) == 0) stop("No numeric columns to compute kurtosis.")
  
  # --- Kurtosis functions ---
  kurtosis_standard <- function(vec) {
    vec <- vec[!is.na(vec)]
    n <- length(vec)
    if (n < 1) return(NA_real_)
    m <- mean(vec)
    s <- sd(vec)
    if (s == 0) return(NA_real_)
    kurt <- mean((vec - m)^4) / (s^4)
    if (excess) kurt - 3 else kurt
  }
  
  kurtosis_unbiased <- function(vec) {
    vec <- vec[!is.na(vec)]
    n <- length(vec)
    if (n < 4) return(NA_real_)
    m <- mean(vec)
    s <- sd(vec)
    m4 <- mean((vec - m)^4)
    num <- n * (n + 1) * m4
    denom <- (n - 1) * (n - 2) * (n - 3) * s^4
    bias_corr <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
    kurt <- (num / denom)
    if (excess) kurt - bias_corr else kurt + (3 - bias_corr)
  }
  
  compute <- if (method == "unbiased") kurtosis_unbiased else kurtosis_standard
  
  result <- vapply(x, compute, numeric(1))
  
  # Output format
  if (length(result) == 1) {
    return(tibble::tibble(kurtosis = result[[1]]))
  } else {
    return(tibble::tibble(variable = names(result), kurtosis = unname(result)))
  }
}
