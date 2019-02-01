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
#' @param type Type of categorical variable: `binary` or `multiple`
#' @param display How to display results: `CP` = counts and proportions, `C` = counts, `P` = proportions
#'     (defaults to `CP`)
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import tibble
#' @return A data frame summarizing counts/proportions of categorical variable at each level of
#'     grouping variable
#' @examples \dontrun{
#' quantify_categorical_edit(covariate = Edu_4cat, obpv_baseline, grouping_var = obpv_quintile,
#' type = 'multiple', display = 'CP')
#' }
#'

quantify_categorical <- function(covariate, df, grouping_var, type = c('multiple', 'binary'),
                                 display = c('CP', 'C', 'P'), digits = 1){

  grouping_var <- enquo(grouping_var)
  covariate <- enquo(covariate)
  cov_name <- quo_name(covariate)

  fil_df <- df %>%
    select(!!grouping_var, !!covariate) %>%
    filter(!is.na(!!covariate))


  var_counts <-  fil_df %>%
    group_by(!!grouping_var, !!covariate) %>%
    summarize(var_counts = n()) %>%
    ungroup()

  num_levels <- df %>% filter(!is.na(!!covariate)) %>% group_by(!!covariate) %>% group_size() %>% length()

  group_var_counts <-  fil_df %>%
    group_by(!!grouping_var) %>%
    group_size() %>%
    rep(each = num_levels) %>%
    as.data.frame() %>%
    rlang::set_names(nm = 'group_counts') %>%
    as_tibble()

  combined <- cbind(var_counts, group_var_counts)

  results <- combined %>% mutate(props = 100 * var_counts / group_counts,
                                 res = paste0(var_counts,
                                              " (",
                                              format(round(props, digits), nsmall = digits),
                                              ")"))

  if (toupper(display) == 'C'){

    results2 <- results %>%
      select(!!grouping_var, !!covariate,  var_counts) %>%
      spread(!!grouping_var, var_counts)
  } else if (toupper(display) == 'P') {

    results2 <- results %>%
      select(!!grouping_var, !!covariate,  props) %>%
      mutate(props = paste0(format(round(props, digits), nsmall = digits))) %>%
      spread(!!grouping_var, props)

  } else {
    results2 <- results %>%
      select(!!grouping_var, !!covariate,  res) %>%
      spread(!!grouping_var, res)
  }


  if (type == 'multiple') {

    # Get rid of first column
    results <- results2 %>% select(-!!covariate)

    #Add a row with blank strings
    row_1 <- rep("", nrow(unique(fil_df[cov_name])))

    #Combine them
    results <- rbind(row_1, results)

    #Add in column named 'var' which includes covariate name and levels

    lev <- levels(as_vector(unique(fil_df[cov_name])))
    var<- c(cov_name, lev)
    res2 <- cbind(var, results, stringsAsFactors = FALSE)

    invisible(res2)

  } else if (type == 'binary') {

    results <- results2 %>% filter(!!covariate == 1) %>% select(-!!covariate)

    var <- as.character(cov_name)
    results <- cbind(var, results, stringsAsFactors = FALSE)

    invisible(results)
  }


}
