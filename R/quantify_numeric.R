#' @title Quantify Numeric Variable
#'
#' @description This function allows you to calculate mean and sd for a numeric variable
#' @param covariate Numeric variable of interest
#' @param df Dataset containing covariate
#' @param grouping_var Variable to group by (will be columns of table)
#' @param show_pval Logical.  Should the p-value results be displayed?
#' @param digits Number of digits to round decimals to
#' @export
#' @import dplyr
#' @importFrom rlang eval_tidy expr warn
#' @importFrom stats anova lm
#' @importFrom tidyr spread
#' @return A data frame summarizing mean/sd of covariate at each level of grouping variable
#' @examples \dontrun{
#' quantify_numeric(covariate = age, df = obpv_baseline, grouping_var = obpv_quintile, digits = 1)
#' }
#'



quantify_numeric <- function(covariate, df, grouping_var, show_pval = TRUE, digits = 1){

  grouping_var <- dplyr::enquo(grouping_var)
  covariate <- dplyr::enquo(covariate)
  cov_name <- dplyr::quo_name(covariate)

  # Filter out NA's, produce warning

  fil_df <- df %>%
    dplyr::select(!!grouping_var, !!covariate) %>%
    dplyr::filter(!is.na(!!covariate))

  num_na <- nrow(df) - nrow(fil_df)

  if(num_na > 0){
    rlang::warn(paste0("There were ", num_na, " NA's removed for ", rlang::eval_tidy(rlang::expr(!!cov_name))))
  }

  # Get p-value from ANOVA test (this is t-test if only two groups)

  pval <- rlang::eval_tidy(

    rlang::expr(stats::anova(stats::lm(!!covariate ~ as.factor(!!grouping_var)))$'Pr(>F)'[1]),

    data = fil_df)

  # Create * flag for p-values < 0.05 and say name of test

  significance <- dplyr::if_else(pval < 0.05, '*', '')
  test <- 'T-test/ANOVA'
  p_value <- format.pval(pval, digits = 2, eps = 0.001, nsmall = 3)
  pv <- cbind(p_value, significance, test)

  # Calculate mean and sd

  res <- fil_df %>%
    dplyr::group_by(!!grouping_var) %>%
    dplyr::summarize(!!cov_name := paste0(format(round(mean(!!covariate), digits), nsmall = digits),
                                          " (",
                                          format(round(sd(!!covariate), digits), nsmall = digits),
                                          ")")) %>%
    tidyr::spread(!!grouping_var, !!cov_name)

  # Combine results

  var <- cov_name

  if (show_pval == TRUE){
    res <- cbind(var, res, pv, stringsAsFactors = FALSE)
  } else {
    res <- cbind(var, res, stringsAsFactors = FALSE)
  }
  invisible(res)
}
