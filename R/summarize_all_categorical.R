#' @title Summarize All Categorical Variables
#'
#' @description This function allows you to calculate counts, proportions, or a combination of both for
#'     all binary or multiple level categorical variables in a dataset.  The function will take two vectors
#'     of categorical variables split by `binary` and `multiple` levels. NOTE: ALL CATEGORICAL VARIABLES
#'     MUST BE CONVERTED TO FACTORS BEFORE USING THIS FUNCTION. For binary variables, make sure vairables
#'     are coded as 0-1.  This function will produce output for the case where the binary variable equals 1.
#'     If both levels of the binary variable are desired, include the binary variable in the `multiple` level
#'     categorical variable vector. For `display` options: `CP` = counts and proportions, `C` = counts, and
#'     `P` = proportions (defaults to `CP`)
#' @param df Dataset containing covariates of interest
#' @param binary_cat_vars Vector of binary variables (in 0-1 format)
#' @param multiple_cat_vars Vector of multiple level categorical variables
#' @param grouping_var Variable to group by (will be columns of table)
#' @param display How to display results: `CP` = counts and proportions, `C` = counts, `P` = proportions
#'     (defaults to `CP`)
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @return A data frame summarizing mean/sd of covariate at each level of grouping variable
#' @examples \dontrun{
#' summarize_all_categorical(df = obpv_baseline, binary_cat_vars = binary_cat_vars,
#' multiple_cat_vars = multiple_cat_vars, grouping_var = obpv_quintile, display = 'CP', digits = 1)
#' }
#'


summarize_all_categorical <- function(df, binary_cat_vars, multiple_cat_vars, grouping_var,
                                      display = c('CP', 'C', 'P'), digits = 1){

  grouping_var <- dplyr::enquo(grouping_var)



  binary <- purrr::map_dfr(syms(binary_cat_vars), quantify_categorical, df = df,
                    grouping_var = !!grouping_var,
                    type = 'binary', display = display, digits = digits)


  multiple <- purrr::map_dfr(syms(multiple_cat_vars), quantify_categorical, df = df,
                      grouping_var = !!grouping_var,
                      type = 'multiple', display = display, digits = digits)

  res <- rbind(binary, multiple)

  invisible(res)
}
