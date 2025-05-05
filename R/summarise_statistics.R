#' @title Summarise Descriptive Statistics with Optional Testing
#'
#' @description
#' Computes descriptive statistics for numeric data. Optionally groups by a variable and includes Shapiro-Wilk and group significance testing. Can color console output for significant differences.
#'
#' @param data A numeric vector, matrix, or data frame.
#' @param group_var Optional. A character name of a grouping variable.
#' @param normality_test Logical. If TRUE, performs Shapiro-Wilk test for normality.
#' @param group_test Logical. If TRUE and `group_var` is set, performs group-wise significance tests (t-test, ANOVA, etc.).
#' @param show_colors Logical. If TRUE and `group_test` is TRUE, prints colored console output for significant group results. Default is TRUE.
#'
#' @return A tibble with descriptive statistics and optional test results per numeric variable.
#'
#' @examples
#' summarise_statistics(iris, group_var = "Species", group_test = TRUE)
#'
#' @rdname summarise_statistics
#' @export
summarise_statistics <- function(data,
                                 group_var = NULL,
                                 normality_test = FALSE,
                                 group_test = FALSE,
                                 show_colors = TRUE) {
  if (is.vector(data) && is.numeric(data)) {
    data <- tibble::tibble(value = data)
  } else if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  
  data <- tibble::as_tibble(data)
  
  # Check if grouping is valid
  if (!is.null(group_var)) {
    if (!group_var %in% names(data)) stop("Grouping variable not found.")
  }
  
  numeric_data <- dplyr::select(data, where(is.numeric))
  if (ncol(numeric_data) == 0) stop("No numeric columns found.")
  
  # Perform grouped summaries
  if (!is.null(group_var)) {
    summary_tbl <- data |>
      dplyr::group_by(.data[[group_var]]) |>
      dplyr::group_modify(~ {
        group_val <- unique(.x[[group_var]])[1]
        purrr::imap_dfr(
          dplyr::select(.x, where(is.numeric)),
          function(x, varname) {
            x <- stats::na.omit(x)
            q <- stats::quantile(x, probs = c(0.25, 0.75))
            mean_val <- mean(x)
            sd_val <- sd(x)
            cv <- if (mean_val == 0) NA_real_ else sd_val / mean_val
            mad_val <- stats::mad(x)
            iqr <- q[[2]] - q[[1]]
            
            if (normality_test && length(x) >= 3 && length(x) <= 5000) {
              sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
              p_val <- if (!is.null(sw)) sw$p.value else NA_real_
              result <- if (!is.null(sw)) if (p_val < 0.05) "not normal" else "normal" else NA_character_
            } else {
              p_val <- NA_real_
              result <- NA_character_
            }
            
            tibble::tibble(
              !!group_var := group_val,
              variable = varname,
              min = min(x),
              q1 = q[[1]],
              median = median(x),
              q3 = q[[2]],
              max = max(x),
              range = diff(range(x)),
              iqr = iqr,
              mean = mean_val,
              variance = var(x),
              sd = sd_val,
              cv = cv,
              mad = mad_val,
              skewness = summarise_skewness(x)$skewness,
              kurtosis = summarise_kurtosis(x)$kurtosis,
              normality_p_value = p_val,
              normality_result = result
            )
          }
        )
      })
  } else {
    summary_tbl <- purrr::imap_dfr(
      numeric_data,
      function(x, varname) {
        x <- stats::na.omit(x)
        q <- stats::quantile(x, probs = c(0.25, 0.75))
        mean_val <- mean(x)
        sd_val <- sd(x)
        cv <- if (mean_val == 0) NA_real_ else sd_val / mean_val
        mad_val <- stats::mad(x)
        iqr <- q[[2]] - q[[1]]
        
        if (normality_test && length(x) >= 3 && length(x) <= 5000) {
          sw <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
          p_val <- if (!is.null(sw)) sw$p.value else NA_real_
          result <- if (!is.null(sw)) if (p_val < 0.05) "not normal" else "normal" else NA_character_
        } else {
          p_val <- NA_real_
          result <- NA_character_
        }
        
        tibble::tibble(
          group = "All",
          variable = varname,
          min = min(x),
          q1 = q[[1]],
          median = median(x),
          q3 = q[[2]],
          max = max(x),
          range = diff(range(x)),
          iqr = iqr,
          mean = mean_val,
          variance = var(x),
          sd = sd_val,
          cv = cv,
          mad = mad_val,
          skewness = summarise_skewness(x)$skewness,
          kurtosis = summarise_kurtosis(x)$kurtosis,
          normality_p_value = p_val,
          normality_result = result
        )
      }
    )
  }
  
  # Significance testing across groups
  if (!is.null(group_var) && group_test) {
    significance_tbl <- purrr::map_dfr(names(numeric_data), function(var) {
      df <- data[, c(group_var, var)]
      df <- stats::na.omit(df)
      names(df) <- c("group", "value")
      
      n_groups <- length(unique(df$group))
      
      use_normal <- if (normality_test) {
        group_normality <- df |>
          dplyr::group_by(group) |>
          dplyr::summarise(p = tryCatch(shapiro.test(value)$p.value, error = function(e) NA_real_))
        all(group_normality$p >= 0.05, na.rm = TRUE)
      } else {
        FALSE
      }
      
      if (n_groups == 2) {
        test <- tryCatch(
          if (use_normal) t.test(value ~ group, data = df)
          else wilcox.test(value ~ group, data = df),
          error = function(e) NULL
        )
      } else {
        test <- tryCatch(
          if (use_normal) {
            aov_res <- aov(value ~ group, data = df)
            summary(aov_res)[[1]][["Pr(>F)"]][1]
          } else {
            kruskal.test(value ~ group, data = df)
          },
          error = function(e) NULL
        )
      }
      
      p_val <- if (inherits(test, "htest")) test$p.value else if (is.numeric(test)) test else NA_real_
      interp <- if (!is.na(p_val) && p_val < 0.05) "significant difference" else "no significant difference"
      
      tibble::tibble(
        variable = var,
        group_test_p_value = round(p_val, 4),
        group_test_result = interp
      )
    })
    
    summary_tbl <- dplyr::left_join(summary_tbl, significance_tbl, by = "variable")
    
    if (show_colors) {
      cat(crayon::bold("\nSignificance Test Summary:\n"))
      print_table <- summary_tbl |>
        dplyr::distinct(variable, group_test_p_value, group_test_result) |>
        dplyr::mutate(
          group_test_result = ifelse(
            group_test_result == "significant difference",
            crayon::red(group_test_result),
            group_test_result
          ),
          group_test_p_value = ifelse(
            group_test_result == crayon::red("significant difference"),
            crayon::red(group_test_p_value),
            group_test_p_value
          )
        )
      print(print_table, n = Inf)
    }
  }
  
  return(summary_tbl)
}
