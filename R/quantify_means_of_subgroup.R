#' @title Quantify Means of Subgroup
#'
#' @description This function allows you to calculate  mean and sd of a numeric variable by grouping
#'     and specific subgroup variable levels (ie. mean age of those belonging to subgroup 1 or 2 by
#'     levels of grouping variable)
#' @param subgroup Subgroup variable of interest (must be factor)
#' @param mean_var Numeric variable from which to calculate mean and sd
#' @param df Dataset containing covariates
#' @param grouping_var Variable to group by (will be columns of table)
#' @param show_pval Logical.  Should the p-value results be displayed?
#' @param digits Number of digits to round decimals
#' @export
#' @importFrom broom tidy
#' @import dplyr
#' @importFrom rlang abort eval_tidy expr warn
#' @importFrom stats anova lm
#' @importFrom tidyr spread
#' @importFrom purrr as_vector
#' @return A data frame summarizing mean and sd of a numeric variable by grouping and specific subgroup
#'     variable levels
#' @examples \dontrun{
#' quantify_means_of_subgroup(subgroup = Edu_4cat, mean_var = BMI, df = obpv_baseline,
#' grouping_var = obpv_quintile)
#' }
#'

quantify_means_of_subgroup <- function(subgroup, mean_var, df, grouping_var, show_pval = TRUE, digits = 1){

  mean_var <- dplyr::enquo(mean_var)
  subgroup <- dplyr::enquo(subgroup)
  grouping_var <- dplyr::enquo(grouping_var)
  sub_name <- dplyr::quo_name(subgroup)
  mean_name <- dplyr::quo_name(mean_var)

  # Filter out NA's, produce warning

  fil_df <- df %>%
    dplyr::select(!!grouping_var, !!subgroup, !!mean_var) %>%
    dplyr::filter(!is.na(!!subgroup) & !is.na(!!mean_var))


  num_na <- nrow(df) - nrow(fil_df)


  if(num_na > 0){
    na_sub <- rlang::eval_tidy(rlang::expr(sum(is.na(!!subgroup))), data = df)
    na_var <- rlang::eval_tidy(rlang::expr(sum(is.na(!!mean_var))), data = df)

    rlang::warn(paste0("There were ", num_na, " total observations removed; ", na_sub, " NA's removed for ", rlang::eval_tidy(rlang::expr(!!sub_name)),
                       " and ", na_var, " NA's removed for ", rlang::eval_tidy(rlang::expr(!!mean_name))))
  }

  # Calculate mean and sd NOW ADD P-VALUE and option for counts/proportions if factor var

  if (rlang::eval_tidy(rlang::expr(is.numeric(!!mean_var)), data = fil_df)){

    # Calculate mean/sd

    calc <- fil_df %>%
      dplyr::group_by(!!grouping_var, !!subgroup) %>%
      dplyr::summarize(res = paste0(format(round(mean(!!mean_var), digits), nsmall = digits),
                                    " (",
                                    format(round(sd(!!mean_var), digits), nsmall = digits),
                                    ")")) %>%
      tidyr::spread(!!grouping_var, res)

    # Calculate p-value and sig flag

    pval <- fil_df %>% dplyr::group_by(!!subgroup) %>%
      dplyr::group_map( ~ broom::tidy(rlang::eval_tidy(
        rlang::expr(stats::anova(stats::lm(!!mean_var ~ as.factor(!!grouping_var), data = .x))),
        data = .x))) %>%
      dplyr::filter(!is.na(p.value)) %>% dplyr::ungroup() %>% dplyr::select(p.value) %>% as.matrix()


    significance <- dplyr::if_else(pval < 0.05, '*', '')
    test <- 'T-test/ANOVA'
    p_value <- format.pval(pval, digits = 2, eps = 0.001, nsmall = 3)
    pv <- cbind(p_value, significance, test)

  } else {
    rlang::abort(paste0("The variable being calculated on must be of class numeric.  Note: class(",
                        rlang::eval_tidy(rlang::expr(!!calc_name), data = fil_df), ") = ",
                        class(rlang::eval_tidy(rlang::expr(!!mean_var), data = fil_df)), "."))
  }



  # Get rid of first column
  results <- calc  %>% dplyr::select(-!!subgroup)

  #Add a row with blank strings
  row_1 <- ''


  #Combine them
  results <- rbind(row_1, results)


  #Add in column named 'var' which includes covariate name and levels
  lev <- levels(purrr::as_vector(unique(fil_df[sub_name])))


  var <- c(paste0(sub_name, " [mean/sd(", mean_name,")]"), lev)

  blanks <- matrix(data = '', nrow = 1, ncol = dim(pv)[2])


  new_pv <- rbind(blanks, pv)

  if (show_pval == TRUE){
    final_res <- cbind(var, results, new_pv, stringsAsFactors = FALSE)
  } else {
    final_res <- cbind(var, results, stringsAsFactors = FALSE)
  }

  invisible(final_res)

}
