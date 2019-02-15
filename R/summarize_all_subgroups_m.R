#' @title Summarize All Subgroups (mean/sd)
#'
#' @description This function allows you to calculate  mean and sd of a numeric variable by grouping
#'     and all subgroup variables of interest (ie. mean age of those belonging to subgroup 1 or 2 by
#'     levels of grouping variable, mean BMI of of those belonging to subgroup 1 or 2 by
#'     levels of grouping variable, etc.)
#' @param df Dataset containing covariates of interest
#' @param subgroups_m Vector of subgroup variables of interest (must be factors)
#' @param mean_vars Vector of numeric variables from which to calculate mean and sd (must match vector
#'     position of corresponding subgroup variable)
#' @param grouping_var Variable to group by (will be columns of table)
#' @param num_display How should results be displayed? ('PM' for mean +- sd, 'PRS' for mean (sd))
#' @param show_pval Logical.  Should the p-value results be displayed?
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @importFrom purrr map2_dfr
#' @return A data frame summarizing mean and sd of a numeric variable by grouping and all subgroup
#'     variables of interest
#' @examples \dontrun{
#' # First create vectors of subgroups and mean_vars, positions must match
#' (ie. mean BMI of levels of Edu_4cat, mean age of levels of sub_senior, etc.)
#'
#' subgroups_m <- c('Edu_4cat', 'sub_senior', 'female', 'race_black')
#' mean_vars <- c('BMI', 'age', 'sbp', 'BMI')
#'
#' # Then run function
#'
#' summarize_all_subgroups_m(df = obpv_baseline, subgroups_m = subgroups_m, mean_vars = mean_vars,
#' grouping_var = obpv_quintile, digits = 2)
#'
#' }
#'



summarize_all_subgroups <- function(df, subgroups_m, mean_vars, grouping_var, num_display = 'PM', show_pval = TRUE, digits = 1){

  grouping_var <- dplyr::enquo(grouping_var)

  sub <- purrr::map2_dfr(syms(subgroups_m), syms(mean_vars), quantify_means_of_subgroup, df = df,
                  grouping_var = !!grouping_var, num_display = num_display, show_pval = show_pval, digits = digits)

  invisible(sub)
}
