#' @title Quantify Counts of Subgroup
#'
#' @description This function allows you to calculate  the counts and proportions of a binary variable
#'     by grouping and specific subgroup variable levels (ie. counts/proportions of depressed males and
#'     females by levels of grouping variable, say treatment arm)
#' @param subgroup_c Subgroup variable of interest (must be factor)
#' @param count_var Factor variable from which to calculate counts and proportions
#' @param df Dataset containing covariates
#' @param grouping_var Variable to group by (will be columns of table)
#' @param count_display How to display results: `CP` = counts and proportions, `C` = counts, `P` = proportions
#'     (defaults to `CP`)
#' @param show_pval Logical.  Should the p-value results be displayed?
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @importFrom rlang abort eval_tidy expr warn
#' @importFrom stats chisq.test
#' @importFrom tibble remove_rownames
#' @importFrom tidyr spread
#' @importFrom purrr pmap_dfr as_vector
#' @return A data frame summarizing counts and proportions of a binary variable by grouping and specific subgroup
#'     variable levels
#' @examples \dontrun{
#' quantify_counts_of_subgroup(grouping_var = randAssign, subgroup_c = depress, count_var = female,
#' count_display = 'CP', show_pval = TRUE, df = baseline, digits = 1)
#' }
#'

quantify_counts_of_subgroup <- function(subgroup_c, count_var, df, grouping_var, count_display = 'CP',
                                        show_pval = FALSE, digits = 1){

  grouping_var <- dplyr::enquo(grouping_var)
  subgroup_c <- dplyr::enquo(subgroup_c)
  count_var <- dplyr::enquo(count_var)
  group_name <- dplyr::quo_name(grouping_var)
  sub_name <- dplyr::quo_name(subgroup_c)
  count_name <- dplyr::quo_name(count_var)

  # Filter out NA's and cast warning

  fil_df <- df %>%
    dplyr::select(!!grouping_var, !!subgroup_c, !!count_var) %>%
    dplyr::filter(!is.na(!!subgroup_c) & !is.na(!!count_var))


  num_na <- nrow(df) - nrow(fil_df)


  if(num_na > 0){
    na_sub <- rlang::eval_tidy(rlang::expr(sum(is.na(!!subgroup_c))), data = df)
    na_var <- rlang::eval_tidy(rlang::expr(sum(is.na(!!count_var))), data = df)

    rlang::warn(paste0("There were ", num_na, " total observations removed; ", na_sub, " NA's removed for ", rlang::eval_tidy(rlang::expr(!!sub_name)),
                       " and ", na_var, " NA's removed for ", rlang::eval_tidy(rlang::expr(!!count_name))))
  }

  # Re-define df so code below can be preserved

  df <- fil_df

  # Conduct chi-sq test

  pval <- df %>% dplyr::group_by(!!subgroup_c) %>%
    dplyr::group_map(~ as.data.frame(stats::chisq.test(unlist(.x[count_name]), as.factor(unlist(.x[group_name])), correct = FALSE)$'p.value')) %>%
    dplyr::ungroup()
  pval <- unlist(pval[ , 2], use.names = FALSE)

  # Create * flag for p-values < 0.05 and say name of test

  significance <- dplyr::if_else(pval < 0.05, '*', '')
  test <- 'Chi-square Test'
  p_value <- format.pval(pval, digits = 2, eps = 0.001, nsmall = 3)
  pv <- cbind(p_value, significance, test)

  # Get combos of group levels

  group_lev <- sort(unlist(unique(df[group_name])))
  subgroup_lev <- sort(unlist(unique(df[sub_name])))
  count_var_lev <- sort(unlist(unique(df[count_name])))
  tot_len <- length(group_lev) * length(subgroup_lev) * length(count_var_lev)


  g_c <- rep(group_lev, each = tot_len / length(group_lev))
  s_c <- rep(subgroup_lev, each = length(count_var_lev), length.out = tot_len)
  c_c <- rep(count_var_lev, tot_len / length(count_var_lev))

  count_d <- data.frame(g_c, s_c, c_c)

  # Function to get counts of level combos

  subgroup_counts <- function(grouping_var_val, subgroup_val, count_val,
                              grouping_var, subgroup_c, count_var, df){

    grouping_var <- dplyr::enquo(grouping_var)
    subgroup_c <- dplyr::enquo(subgroup_c)
    count_var <- dplyr::enquo(count_var)

    res <- df %>% dplyr::filter(!!grouping_var == grouping_var_val, !!subgroup_c == subgroup_val) %>%
      dplyr::filter(!!count_var == count_val) %>% summarize(n = n())
    invisible(res)
  }

  # Map over level combos
  counts <- purrr::pmap_dfr(list(g_c, s_c, c_c), subgroup_counts,
                            grouping_var = !!grouping_var, subgroup_c = !!subgroup_c,
                            count_var = !!count_var, df = df)

  fin <- cbind(count_d, counts)

  #Combine and select only where count_var == 1 (characteristic of interest)

  over <- fin %>% dplyr::group_by(g_c, s_c) %>% dplyr::summarize(overall = sum(n))
  red <- fin %>% dplyr::group_by(g_c, s_c) %>% dplyr::filter(c_c == 1)

  res <- over %>% dplyr::left_join(red, by = c("g_c", "s_c")) %>%
    dplyr::select(-c_c) %>% dplyr::mutate(perc = format(round(100 * n / overall, digits), nsmall = digits),
                            cn = paste0(n, "/", overall),
                            cp = paste0(cn, " (", perc, ")"))


  if (toupper(count_display) == 'CP' | is.null(count_display)){
    res2 <- res %>% dplyr::select(g_c, s_c, cp) %>% tidyr::spread(g_c, cp)
  } else if (toupper(count_display) == 'C'){
    res2 <- res %>% dplyr::select(g_c, s_c, cn) %>% tidyr::spread(g_c, cn)
  } else if (toupper(count_display) == 'P') {
    res2 <- res %>% dplyr::select(g_c, s_c, perc) %>% tidyr::spread(g_c, perc)
  } else {
    rlang::abort("Incorrect specification for categorical display. (Suggest: 'CP','C', or 'P')")
  }

  results <- res2 %>% dplyr::select(-s_c)

  #Add a row with blank strings
  row_1 <- ''

  #Combine them
  results <- rbind(row_1, results)

  #Add in column named 'var' which includes covariate name and levels

  lev <- levels(purrr::as_vector(unique(df[sub_name])))

  var <- c(paste0(sub_name, " [count/prop(", count_name,")]"), lev)

  new_pv <- rbind(row_1, pv)

  if (show_pval == TRUE){
    final_res <- cbind(var, results, new_pv, stringsAsFactors = FALSE)
  } else {
    final_res <- cbind(var, results,  stringsAsFactors = FALSE)
  }

  final_res <- tibble::remove_rownames(final_res)
  invisible(final_res)

}
