#' Core function that combines datasets for comparison.
#'
#' This function is the first step in reconciling datasets. It mashes together the datasets that need to be reconciled.
#' @param first_dataset Data.frame of the format (primary_key, value).
#' @param second_dataset Data.frame of the format (primary_key, value).
#' @param name_vector Vector of length 2 to rename the computed columns.
#' @keywords Data merge, diffing, comparison, reconciliation
#' @export
#' @examples
#' merge_datasets()

merge_datasets <- function(first_dataset, second_dataset, name_vector = NA) {
  colnames(first_dataset) <- c("group_col", "compare_col")
  colnames(second_dataset) <- colnames(first_dataset)
  first_dataset$group_col <- as.factor(first_dataset$group_col)
  second_dataset$group_col <- as.factor(second_dataset$group_col)
  merged_dataset <- full_join(first_dataset,second_dataset,by = c("group_col"))
  colnames(merged_dataset) = c("group_col","compare_col_f1")
  if(is.na(name_vector) == F){
    colnames(merged_dataset) <- name_vector
  } 
  return(merged_dataset)
};
