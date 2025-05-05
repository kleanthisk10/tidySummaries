#' @importFrom stats sd cor cor.test aov kruskal.test shapiro.test t.test wilcox.test median var
#' @importFrom dplyr all_of
#' @import rlang

NULL

utils::globalVariables(c(
  "variable", "value", "correlation", "p_value", "frequency",
  "group_test_result", "group_test_p_value", ".group", "group","N", ".data"
))