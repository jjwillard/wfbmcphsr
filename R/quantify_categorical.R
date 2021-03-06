#' @title Quantify Categorical Variable
#'
#' @description This function allows you to calculate counts, proportions, or a combination of both for
#'     binary or multiple level categorical variables. NOTE: ALL CATEGORICAL VARIABLES MUST BE CONVERTED
#'     TO FACTORS BEFORE USING THIS FUNCTION. For binary variables, make sure vairables are coded as 0-1
#'     and choose `type = 'binary'`.  This will produce output for the case where the variable equals 1.
#'     If both levels of the binary variable are desired, choose `type = 'multiple'` which produces
#'     output for all levels of the categorical variable.
#' @param covariate Categorical variable of interest
#' @param df Dataset containing covariate
#' @param grouping_var Variable to group by (will be columns of table)
#' @param type Type of categorical variable: `binary` or `multiple` (Note: If you would like to see both levels of
#'     a binary variable in the output, then specify `multiple`).
#' @param display How to display results: `CP` = counts and proportions, `C` = counts, `P` = proportions
#'     (defaults to `CP`)
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @importFrom rlang abort eval_tidy expr warn
#' @importFrom stats chisq.test
#' @importFrom tidyr spread
#' @importFrom tibble as_tibble
#' @importFrom purrr set_names as_vector
#' @return A data frame summarizing counts/proportions of categorical variable at each level of
#'     grouping variable
#' @examples \dontrun{
#' quantify_categorical(covariate = Edu_4cat, obpv_baseline, grouping_var = obpv_quintile,
#' type = 'multiple', display = 'CP')
#' }
#'

quantify_categorical <- function(covariate, df, grouping_var, type = c('multiple', 'binary'),
                                 display = 'CP', show_pval = TRUE, digits = 1){

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

  # Get p-value from Chi-square test
  pval <- rlang::eval_tidy(

    rlang::expr(stats::chisq.test(!!covariate, as.factor(!!grouping_var), correct = FALSE)$'p.value'),

    data = fil_df)


  # Create * flag for p-values < 0.05 and say name of test

  significance <- dplyr::if_else(pval < 0.05, '*', '')
  test <- 'Chi-square Test'
  p_value <- format.pval(pval, digits = 2, eps = 0.001, nsmall = 3)
  pv <- cbind(p_value, significance, test)


  # Get counts and proportions

  var_counts <-  fil_df %>%
    dplyr::group_by(!!grouping_var, !!covariate) %>%
    dplyr::summarize(var_counts = n()) %>%
    dplyr::ungroup()

  num_levels <- df %>% dplyr::filter(!is.na(!!covariate)) %>%
    dplyr::group_by(!!covariate) %>%
    dplyr::group_size() %>%
    length()

  group_var_counts <-  fil_df %>%
    dplyr::group_by(!!grouping_var) %>%
    dplyr::group_size() %>%
    rep(each = num_levels) %>%
    as.data.frame() %>%
    purrr::set_names(nm = 'group_counts') %>%
    tibble::as_tibble()

  combined <- cbind(var_counts, group_var_counts)

  results <- combined %>% dplyr::mutate(props = 100 * var_counts / group_counts,
                                        res = paste0(var_counts,
                                                     " (",
                                                     format(round(props, digits), nsmall = digits),
                                                     ")"))

  # How should the results be displayed?

  if (toupper(display) == 'CP' | is.null(display)){
    results2 <- results %>%
      dplyr::select(!!grouping_var, !!covariate,  res) %>%
      tidyr::spread(!!grouping_var, res)
  } else if (toupper(display) == 'C'){

    results2 <- results %>%
      dplyr::select(!!grouping_var, !!covariate,  var_counts) %>%
      tidyr::spread(!!grouping_var, var_counts)
  } else if (toupper(display) == 'P') {

    results2 <- results %>%
      dplyr::select(!!grouping_var, !!covariate,  props) %>%
      dplyr::mutate(props = paste0(format(round(props, digits), nsmall = digits))) %>%
      tidyr::spread(!!grouping_var, props)

  } else {
    rlang::abort('Incorrect specification for categorical display.')
  }

  # What kind of variable is this?

  if (type == 'multiple') {

    # Get rid of first column
    results <- results2 %>% dplyr::select(-!!covariate)

    #Add a row with blank strings
    row_1 <- ''

    #Combine them
    results <- rbind(row_1, results)

    #Add in column named 'var' which includes covariate name and levels

    lev <- levels(purrr::as_vector(unique(fil_df[cov_name])))
    var<- c(cov_name, lev)
    blanks <- matrix(data = '', nrow = length(lev), ncol = length(pv))
    new_pv <- rbind(pv, blanks)


    if (show_pval == TRUE){
      res2 <- cbind(var, results, new_pv, stringsAsFactors = FALSE)
    } else {
      res2 <- cbind(var, results, stringsAsFactors = FALSE)
    }

    invisible(res2)

  } else if (type == 'binary') {

    results <- results2 %>% dplyr::filter(!!covariate == 1) %>% dplyr::select(-!!covariate)

    var <- as.character(cov_name)


    if (show_pval == TRUE){
      results <- cbind(var, results, pv, stringsAsFactors = FALSE)
    } else {
      results <- cbind(var, results, stringsAsFactors = FALSE)
    }

    invisible(results)
  }


}

