#' @title Multiple Pattern-Replacement Substitutions
#'
#' @description
#' Applies multiple regular expression substitutions to a character vector or a specific column of a data frame. 
#' Performs replacements sequentially
#'
#' @param x A character vector or a data frame containing the text to modify.
#' @param pattern A character vector of regular expressions to match.
#' @param replacement A character vector of replacement strings, same length as `pattern`.
#' @param column Optional. If `x` is a data frame, the name of the character column to apply the replacements to.
#' @param ... Additional arguments passed to `gsub()`, such as `ignore.case = TRUE`.
#'
#' @return 
#' - If `x` is a character vector, returns a modified character vector.
#' - If `x` is a data frame, returns the data frame with the specified column modified.
#'
#' @examples
#' # Example on a character vector
#' text <- c("The cat and the dog", "dog runs fast", "no animals")
#' str_replace_many(text, pattern = c("cat", "dog"), replacement = c("lion", "wolf"))
#'
#' # Example on a data frame
#' library(tibble)
#' df <- tibble(id = 1:3, text = c("The cat sleeps", "dog runs fast", "no pets"))
#' str_replace_many(df, pattern = c("cat", "dog"), replacement = c("lion", "wolf"), column = "text")
#'
#' @rdname str_replace_many
#' @export


str_replace_many <- function(x, pattern, replacement, column = NULL, ...) {
  if (length(pattern) != length(replacement)) {
    stop("The 'pattern' and 'replacement' vectors must have the same length.")
  }
  
  if (is.data.frame(x)) {
    if (is.null(column)) {
      stop("When providing a data frame, you must specify the 'column' to modify.")
    }
    if (!column %in% names(x)) {
      stop("Column '", column, "' not found in the data frame.")
    }
    vec <- as.character(x[[column]])
  } else {
    vec <- as.character(x)
  }
  
  # Sequential replacement
  for (i in seq_along(pattern)) {
    vec <- gsub(pattern[i], replacement[i], vec, ...)
  }
  
  # Return depending on input type
  if (is.data.frame(x)) {
    x[[column]] <- vec
    return(x)
  } else {
    return(vec)
  }
}
