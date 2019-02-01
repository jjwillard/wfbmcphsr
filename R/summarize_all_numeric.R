#' @title Summarize All Numeric Variables
#'
#' @description This function allows you to calculate mean and sd for all numeric variables in a dataset
#' @param df Dataset containing covariates of interest
#' @param num_vars Vector of numeric variables
#' @param grouping_var Variable to group by (will be columns of table)
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @return A data frame summarizing mean/sd of covariate at each level of grouping variable
#' @examples \dontrun{
#' quantify_numeric(covariate = age, df = obpv_baseline, grouping_var = obpv_quintile, digits = 1)
#' }
#'



summarize_all_numeric <- function(df, num_vars, grouping_var, digits = 1){

  grouping_var <- enquo(grouping_var)

  res <- purrr::map_dfr(syms(num_vars), quantify_numeric, df = df,
                 grouping_var = !!grouping_var,
                 digits = digits)
  invisible(res)
}
