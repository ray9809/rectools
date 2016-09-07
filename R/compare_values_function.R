#' Core function that diffs the values of a merged dataset.
#'
#' This function is the second step in reconciling datasets. It compares the values on a merged dataset.
#' @param merged_dataset Data.frame of the format (primary_key, value_from_first, value_from_second).
#' @param name_vector Vector of length 3 to rename the computed columns.
#' @keywords Data merge, diffing, comparison, reconciliation
#' @export
#' @examples
#' compare_values()

compare_values <- function(merged_dataset, name_vector = NA) {
  colnames(merged_dataset) <- c("group_col", "compare_col_f1","compare_col_f2");
  
  merged_dataset <- merged_dataset %>%
    group_by(group_col) %>%
    mutate(on_f1 = ifelse(is.na(compare_col_f1)==T,0,1)) %>%
    mutate(on_f2 = ifelse(is.na(compare_col_f2)==T,0,1)) %>%
    mutate(diff = ifelse(is.na(compare_col_f1)==T,0,compare_col_f1) - ifelse(is.na(compare_col_f2)==T,0,compare_col_f2)) %>%
    mutate(scale = 
             ifelse(
               ifelse(is.na(compare_col_f2)==T,0,compare_col_f2) == 0, 1000, ifelse(
                 ifelse(is.na(compare_col_f1)==T,0,compare_col_f1) == 0, 1000, ifelse(
                   compare_col_f2/compare_col_f1 == 1, 1, ifelse(
                     compare_col_f2/compare_col_f1 > 1, compare_col_f2/compare_col_f1, ifelse(
                       compare_col_f2/compare_col_f1 < 1, compare_col_f1/compare_col_f2,0
                     )))))
    ) %>%
    mutate(diff_type = 
             factor(
               ifelse(
                 on_f1 == 0, "missing from f1", ifelse(
                   on_f2 == 0, "missing from f2", ifelse(
                     abs(diff) == 0, "no difference", ifelse(
                       abs(diff) <= 0.05, "possible rounding error", ifelse(
                         abs(diff) > 0.05 & round(scale,2) == 2, "duplication", "calculation difference"
                       ))))),
               levels=c("missing from f1","missing from f2","no difference","possible rounding error","duplication","calculation difference"))
    )
  
  if(is.na(name_vector) == F){
    colnames(f3) <- name_vector
  } 
  return(f3)
}