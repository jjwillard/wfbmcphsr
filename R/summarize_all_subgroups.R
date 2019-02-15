#' @title Summarize All Subgroups
#'
#' @description This function allows you to calculate  mean and sd of a numeric variable by grouping
#'     and all subgroup variables of interest (ie. mean age of those belonging to subgroup 1 or 2 by
#'     levels of grouping variable, mean BMI of of those belonging to subgroup 1 or 2 by
#'     levels of grouping variable, etc.)
#' @param df Dataset containing covariates of interest
#' @param subgroups_m Vector of subgroup variables of interest (must be factors)
#' @param mean_vars Vector of numeric variables from which to calculate mean and sd (must match vector
#'     position of corresponding subgroup variable)
#' @param num_display How should results be displayed? ('PM' for mean +- sd, 'PRS' for mean (sd))
#' @param subgroups_c Subgroup variable of interest (must be factor)
#' @param count_vars Factor variable from which to calculate counts and proportions
#' @param count_display How to display results: `CP` = counts and proportions, `C` = counts, `P` = proportions
#'     (defaults to `CP`)
#' @param grouping_var Variable to group by (will be columns of table)
#' @param show_pval Logical.  Should the p-value results be displayed?
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @importFrom purrr map2_dfr
#' @return A data frame summarizing mean and sd/count and proportion of a variable by grouping and all subgroup
#'     variables of interest
#' @examples \dontrun{
#' # First create vectors of subgroups and calculation variables, positions must match
#' (ie. mean BMI of levels of Edu_4cat, mean age of levels of sub_senior, etc.)
#'
#' subgroups_m <- c('Edu_4cat_m', 'sub_senior_m', 'female_m', 'race_black_m')
#' mean_vars <- c('BMI', 'age', 'sbp', 'BMI')
#'
#' subgroups_c <- c('female_c', 'sub_senior_c')
#' count_vars <- c('depress', 'race_black')
#'
#' # Then run function
#'
#' summarize_all_subgroups(df = obpv_baseline, subgroups_m = subgroups_m, mean_vars = mean_vars,
#' num_display = 'PM', subgroups_c = subgroups_c, count_vars = count_vars, count_display = 'CP',
#' grouping_var = obpv_quintile, digits = 2)
#'
#' }
#'



summarize_all_subgroups <- function(df, subgroups_m = NULL, mean_vars = NULL, num_display = 'PM',
                                    subgroups_c = NULL, count_vars = NULL, count_display = 'CP',
                                    grouping_var, show_pval = TRUE, digits = 1){

  grouping_var <- dplyr::enquo(grouping_var)


  sub_m <- purrr::map2_dfr(syms(subgroups_m), syms(mean_vars), quantify_means_of_subgroup, df = df,
                           grouping_var = !!grouping_var, num_display = num_display, show_pval = show_pval, digits = digits)

  sub_c <- purrr::map2_dfr(syms(subgroups_c), syms(count_vars), quantify_counts_of_subgroup, df = df,
                           grouping_var = !!grouping_var, count_display = count_display, show_pval = show_pval, digits = digits)

  res <- rbind(sub_m, sub_c)

  invisible(res)
}
