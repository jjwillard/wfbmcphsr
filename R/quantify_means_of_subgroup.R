#' @title Quantify Means of Subgroup
#'
#' @description This function allows you to calculate  mean and sd of a numeric variable by grouping
#'     and specific subgroup variable levels (ie. mean age of those belonging to subgroup 1 or 2 by
#'     levels of grouping variable)
#' @param subgroup Subgroup variable of interest (must be factor)
#' @param mean_var Numeric variable from which to calculate mean and sd
#' @param df Dataset containing covariates
#' @param grouping_var Variable to group by (will be columns of table)
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @import tidyr
#' @return A data frame summarizing mean and sd of a numeric variable by grouping and specific subgroup
#'     variable levels
#' @examples \dontrun{
#' quantify_means_of_subgroup(subgroup = Edu_4cat, mean_var = BMI, df = obpv_baseline,
#' grouping_var = obpv_quintile)
#' }
#'

quantify_means_of_subgroup <- function(subgroup, mean_var, df, grouping_var, digits = 1){

  mean_var <- enquo(mean_var)
  subgroup <- enquo(subgroup)
  grouping_var <- enquo(grouping_var)
  sub_name <- quo_name(subgroup)
  mean_name <- quo_name(mean_var)

  fil_df <- df %>%
    select(!!grouping_var, !!subgroup, !!mean_var) %>%
    filter(!is.na(!!subgroup) & !is.na(!!mean_var))


  calc <- fil_df %>%
    group_by(!!grouping_var, !!subgroup) %>%
    summarize(res = paste0(format(round(mean(!!mean_var), digits), nsmall = digits),
                           " (",
                           format(round(sd(!!mean_var), digits), nsmall = digits),
                           ")")) %>%
    spread(!!grouping_var, res)


  # Get rid of first column
  results <- calc  %>% select(-!!subgroup)

  #Add a row with blank strings
  row_1 <- rep("", nrow(unique(fil_df[sub_name])))


  #Combine them
  results <- rbind(row_1, results)

  #Add in column named 'var' which includes covariate name and levels
  lev <- sort(unique(as_vector(fil_df[sub_name])))
  var <- c(paste0(sub_name, "[mean(", mean_name,")]"), lev)
  final_res <- cbind(var, results , stringsAsFactors = FALSE)



  invisible(final_res)

}
