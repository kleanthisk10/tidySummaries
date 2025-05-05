#' @title Summarize Grouped Statistics
#'
#' @description
#' Groups a data frame by one or more variables and summarizes the selected numeric columns using basic statistic functions.
#' Handles missing values by replacement with zero or removal of rows.
#'
#' @param df A data frame or tibble containing the data.
#' @param group_var A character vector of column names to group by.
#' @param values A character vector of numeric column names to summarize.
#' @param m_functions A character vector of functions to apply (e.g., "mean", "sd", "length"). Default is c("mean", "sd", "length").
#' @param replace_na Logical. If TRUE, missing values in numeric columns are replaced with 0. Default is FALSE.
#' @param remove_na Logical. If TRUE, rows with missing values in group or value columns are removed. Default is FALSE.
#'
#' @return A tibble with grouped and summarized results.
#'
#' @examples
#' summarise_group_stats(iris, group_var = "Species",
#'  values = c("Sepal.Length", "Petal.Width"))
#' summarise_group_stats(mtcars, 
#' group_var = c("cyl", "gear"), 
#' values = c("mpg", "hp"), remove_na = TRUE)
#'
#' @rdname summarise_group_stats
#' @export

summarise_group_stats <- function(df,
                                  group_var,
                                  values,
                                  m_functions = c("mean", "sd", "length"),
                                  replace_na = FALSE,
                                  remove_na = FALSE) {
  df <- tibble::as_tibble(df)
  
  # Validate column existence
  missing_group_vars <- setdiff(group_var, names(df))
  missing_values <- setdiff(values, names(df))
  
  if (length(missing_group_vars) > 0) {
    stop("Grouping variable(s) not found: ", paste(missing_group_vars, collapse = ", "))
  }
  if (length(missing_values) > 0) {
    stop("Value column(s) not found: ", paste(missing_values, collapse = ", "))
  }
  
  # Handle missing values
  if (remove_na) {
    df <- dplyr::filter(df, dplyr::if_all(dplyr::all_of(c(group_var, values)), ~ !is.na(.)))
  }
  if (replace_na) {
    df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(values), ~ tidyr::replace_na(., 0)))
  }
  
  # Convert character vector to named list of functions
  fun_list <- rlang::set_names(m_functions) |>
    purrr::map(~ match.fun(.x))
  
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_var))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(values),
        .fns = fun_list,
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}
