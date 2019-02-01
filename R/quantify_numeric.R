#' @title Quantify Numeric Variable
#'
#' @description This function allows you to calculate mean and sd for a numeric variable
#' @param covariate Numeric variable of interest
#' @param df Dataset containing covariate
#' @param grouping_var Variable to group by (will be columns of table)
#' @param digits Number of digits to round decimals to
#' @export
#' @import dplyr
#' @import tidyr
#' @return A data frame summarizing mean/sd of covariate at each level of grouping variable
#' @examples \dontrun{
#' quantify_numeric(covariate = age, df = obpv_baseline, grouping_var = obpv_quintile, digits = 1)
#' }
#'

quantify_numeric <- function(covariate, df, grouping_var, digits = 1){

  grouping_var <- enquo(grouping_var)
  covariate <- enquo(covariate)
  cov_name <- quo_name(covariate)

  fil_df <- df %>%
    select(!!grouping_var, !!covariate) %>%
    filter(!is.na(!!covariate))


  res <- fil_df %>%
    group_by(!!grouping_var) %>%
    summarize(!!cov_name := paste0(format(round(mean(!!covariate), digits), nsmall = digits),
                                   " (",
                                   format(round(sd(!!covariate), digits), nsmall = digits),
                                   ")")) %>%
    spread(!!grouping_var, !!cov_name)


  var <- cov_name

  res <- cbind(var, res, stringsAsFactors = FALSE)

  invisible(res)
}
