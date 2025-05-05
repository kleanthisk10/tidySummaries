#' @title Select Numeric Columns
#'
#' @description
#' Returns a tibble with only the numeric columns of the input, and optionally drops rows with NAs.
#'
#' @param dataset A vector, matrix, data frame, or tibble.
#' @param remove_na Logical. If TRUE, rows with any NA values will be dropped. Default is FALSE.
#'
#' @return A tibble with only numeric columns.
#'
#' @examples
#' select_numeric_cols(iris)
#' @rdname select_numeric_cols
#' @export
select_numeric_cols <- function(dataset, remove_na = FALSE) {
  dataset <- tibble::as_tibble(dataset)
  
  result <- dataset %>%
    dplyr::select(dplyr::where(is.numeric))
  
  if (remove_na) {
    result <- tidyr::drop_na(result)
  }
  
  return(result)
}
