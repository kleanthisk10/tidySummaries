#' @title Select Non-Numeric Columns
#'
#' @description
#' Returns a tibble with only the non-numeric columns of the input, and optionally drops rows with NAs.
#'
#' @param dataset A vector, matrix, data frame, or tibble.
#' @param remove_na Logical. If TRUE, rows with any NA values will be dropped. Default is FALSE.
#'
#' @return A tibble with only non-numeric columns.
#'
#' @examples
#' select_non_numeric_cols(iris)
#'
#' df <- tibble::tibble(a = 1:6, b = c("x", "y", NA, NA, "z", NA))
#' select_non_numeric_cols(df, remove_na = TRUE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select where
#' @importFrom tidyr drop_na
#' @importFrom tibble as_tibble
#' @rdname select_non_numeric_cols
#' @export
select_non_numeric_cols <- function(dataset, remove_na = FALSE) {
  dataset <- tibble::as_tibble(dataset)
  
  result <- dataset %>%
    dplyr::select(dplyr::where(~ !is.numeric(.)))
  
  if (remove_na) {
    result <- tidyr::drop_na(result)
  }
  
  return(result)
}
