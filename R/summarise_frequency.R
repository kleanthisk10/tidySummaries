#' @title Summarise Frequency Table
#'
#' @description
#' Computes the frequency and relative frequency (or percentage) of factor or character variables in a data frame or vector.
#'
#' @param data A character/factor vector, or a data frame/tibble.
#' @param select Optional. One or more variable names to compute frequencies for. If NULL, all factor/character columns are used.
#' @param as_percent Logical. If TRUE, relative frequencies are returned as percentages (\%). Default is FALSE (proportions).
#' @param sort_by Optional. If "N", sorts by frequency; if "group", sorts alphabetically; or "\%N" (if as_percent = TRUE). 
#' Default is no sorting.
#' @param top_n Integer. Show only the top N values
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{variable}{The name of the variable.}
#'   \item{group}{The group/category values of the variable.}
#'   \item{N}{The count (frequency) of each group.}
#'   \item{\%N}{The proportion or percentage of each group.}
#' }
#'
#' @examples
#' summarise_frequency(iris, select = "Species")
#' summarise_frequency(iris, as_percent = TRUE, sort_by = "N", top_n = 2)
#' summarise_frequency(data.frame(group = c("A", "A", "B", "C", "A")), as_percent = TRUE)
#'
#' @rdname summarise_frequency
#' @export
summarise_frequency <- function(data,
                                select = NULL,
                                as_percent = FALSE,
                                sort_by = NULL,
                                top_n = Inf) {
  # Coerce to tibble if vector
  if (is.vector(data) && (is.character(data) || is.factor(data))) {
    data <- tibble::tibble(group = data)
  } else {
    data <- tibble::as_tibble(data)
  }
  
  # Get factor or character variables
  char_vars <- names(data)[sapply(data, function(col) is.character(col) || is.factor(col))]
  if (length(char_vars) == 0) stop("No factor or character columns found.")
  
  # Validate selection
  if (!is.null(select)) {
    select <- match.arg(select, choices = char_vars, several.ok = TRUE)
  } else {
    select <- char_vars
  }
  
  result <- purrr::map_dfr(
    select,
    function(var) {
      tab <- table(data[[var]], useNA = "no")
      rel <- prop.table(tab)
      
      df <- tibble::tibble(
        variable = var,
        group = names(tab),
        N = as.integer(tab),
        `%N` = if (as_percent) {
          paste0(formatC(rel * 100, format = "f", digits = 2), "%")
        } else {
          round(rel, 4)
        }
      )
      
      # Optional sorting
      if (!is.null(sort_by)) {
        if (!sort_by %in% c("N", "%N", "group")) {
          stop("`sort_by` must be one of: 'N', '%N', or 'group'")
        }
        
        if (sort_by == "N") {
          df <- dplyr::arrange(df, variable, dplyr::desc(N))
        } else if (sort_by == "%N") {
          if (!as_percent) stop("You must set `as_percent = TRUE` to sort by '%N'")
          numeric_percent <- as.numeric(sub("%", "", df$`%N`))
          df <- df[order(df$variable, -numeric_percent), ]
        } else if (sort_by == "group") {
          df <- dplyr::arrange(df, variable, group)
        }
      }
      
      # Top N per variable
      if (is.finite(top_n)) {
        df <- df |> dplyr::group_by(variable) |> dplyr::slice_head(n = top_n) |> dplyr::ungroup()
      }
      
      return(df)
    }
  )
  
  return(result)
}
