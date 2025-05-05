#' @title Summarise Correlation Matrix with Optional Significance Tests
#'
#' @description
#' Computes correlations between numeric variables of a data frame, or between two vectors. 
#' Optionally tests statistical significance (p-value)
#'
#' @param x A numeric vector, matrix, data frame, or tibble.
#' @param y Optional. A second numeric vector, matrix, or data frame (same dimensions as `x`).
#' @param method Character. One of "pearson" (default), "kendall", or "spearman".
#' @param cor_test Logical. If TRUE, uses `cor.test()` and includes p-values. If FALSE, uses `cor()` only.
#'
#' @return A tibble with variables, correlations, and optionally p-values.
#' Significant results (p < 0.05) are printed in red in the console.
#'
#' @examples
#' summarise_correlation(iris)
#' summarise_correlation(iris$Sepal.Length, iris$Petal.Length, cor_test = TRUE)
#' 
#' @rdname summarise_correlation
#' @export
#' 
summarise_correlation <- function(x, y = NULL, method = c("pearson", "kendall", "spearman"), cor_test = FALSE) {
  method <- match.arg(method)
  
  # Case 1: Two numeric vectors
  if (is.numeric(x) && is.numeric(y)) {
    if (!cor_test) {
      corr <- stats::cor(x, y, method = method, use = "pairwise.complete.obs")
      result <- tibble::tibble(
        var1 = deparse(substitute(x)),
        var2 = deparse(substitute(y)),
        correlation = round(corr, 3)
      )
    } else {
      test <- stats::cor.test(x, y, method = method)
      result <- tibble::tibble(
        var1 = deparse(substitute(x)),
        var2 = deparse(substitute(y)),
        correlation = round(test$estimate, 3),
        p_value = paste0(formatC(test$p.value, format = "f", digits = 3),
                         ifelse(test$p.value < 0.05, "*", ""))
      )
    }
    
    return(result)
  }
  
  # Coerce input(s) to tibble and select numeric columns
  x <- tibble::as_tibble(x)
  x <- dplyr::select(x, dplyr::where(is.numeric))
  
  if (!is.null(y)) {
    y <- tibble::as_tibble(y)
    y <- dplyr::select(y, dplyr::where(is.numeric))
    if (!all(dim(x) == dim(y))) stop("x and y must have the same dimensions.")
    
    results <- purrr::map2_dfr(names(x), names(y), function(i, j) {
      if (cor_test) {
        test <- stats::cor.test(x[[i]], y[[j]], method = method)
        tibble::tibble(
          var1 = i,
          var2 = j,
          correlation = round(test$estimate, 3),
          p_value = paste0(formatC(test$p.value, format = "f", digits = 3),
                           ifelse(test$p.value < 0.05, "*", ""))
        )
      } else {
        tibble::tibble(
          var1 = i,
          var2 = j,
          correlation = round(stats::cor(x[[i]], y[[j]], method = method, use = "pairwise.complete.obs"), 3)
        )
      }
    })
    
    return(results)
  }
  
  # Case 3: Single data frame (pairwise correlations)
  if (ncol(x) < 2) stop("Need at least two numeric columns.")
  
  vars <- names(x)
  results <- tibble::tibble()
  
  for (i in seq_len(ncol(x) - 1)) {
    for (j in (i + 1):ncol(x)) {
      v1 <- vars[i]
      v2 <- vars[j]
      
      if (cor_test) {
        test <- stats::cor.test(x[[v1]], x[[v2]], method = method)
        results <- dplyr::bind_rows(results, tibble::tibble(
          var1 = v1,
          var2 = v2,
          correlation = round(test$estimate, 3),
          p_value = paste0(formatC(test$p.value, format = "f", digits = 3),
                           ifelse(test$p.value < 0.05, "*", ""))
        ))
      } else {
        corr <- stats::cor(x[[v1]], x[[v2]], method = method, use = "pairwise.complete.obs")
        results <- dplyr::bind_rows(results, tibble::tibble(
          var1 = v1,
          var2 = v2,
          correlation = round(corr, 3)
        ))
      }
    }
  }
  
  return(results)
}
