#' @title Make Table One
#'
#' @description This function allows you to make a Table One for manuscript publication in the
#'     Public Health Sciences
#' @param df Dataset containing covariates
#' @param grouping_var Variable to group by (will be columns of table)
#' @param num_vars Vector of numeric variables
#' @param num_display How should numeric variable results be displayed? ('PM' for mean +- sd, 'PRS' for mean (sd))
#' @param binary_cat_vars Vector of binary variables (in 0-1 format)
#' @param multiple_cat_vars Vector of multiple level categorical variables
#' @param cat_display How to display results: `CP` = counts and proportions, `C` = counts, `P` = proportions
#'     (defaults to `CP`)
#' @param subgroups_m Vector of subgroup variables of interest (must be factors). NOTE:  If you plan to analyze
#'     a categorical variable by itself and then also use it for subgroup analysis, it is advised to copy
#'     and rename the columns of the categorical variables you wish to use as subgroups (ie. rename variable
#'     to something like `variable_sub``) so Table One can be properly sorted to reflect your specification in
#'     `order_of_vars`.  If the subgroup does not have a unique name, it will be sorted directly below the
#'     categorical variable.
#' @param mean_vars_for_subgroups Vector of numeric variables from which to calculate mean and sd (must match vector
#'     position of corresponding subgroup variable)
#' @param order_of_vars A vector of all variables listed in the order of the rows you desire for your
#'     Table One.
#' @param export_rtf Logical. Do you want to export an RTF to location specified in `rtf_filename`?
#' @param rtf_filename Must be quoted and include `.rtf` extension (See example)
#' @param show_pval Logical.  Should the p-value results be displayed?
#' @param digits Number of digits to round decimals
#' @export
#' @import dplyr
#' @importFrom rtf RTF addTable done
#' @importFrom stringr str_split
#' @importFrom tibble tibble remove_rownames
#' @return A sorted Table One including numeric, categorical and subgroup variables
#' @examples \dontrun{
#' make_table_one(df = obpv_baseline, grouping_var = obpv_quintile, num_vars = num_vars,
#' binary_cat_vars = binary_cat_vars, multiple_cat_vars = multiple_cat_vars, display = 'CP',
#' subgroups_m = subgroups, mean_vars_for_subgroups = mean_vars, order_of_vars = var_order,
#' export_rtf = TRUE, rtf_filename = 'my_location/my_filename.rtf', digits = 2)
#' }




make_table_one <- function(df, grouping_var, num_vars = NULL, num_display = 'PM',
                           binary_cat_vars = NULL, multiple_cat_vars = NULL, cat_display = 'CP',
                           subgroups_m = NULL, mean_vars_for_subgroups = NULL,
                           order_of_vars = NULL,
                           export_rtf = FALSE, rtf_filename = NULL,
                           show_pval = TRUE, digits = 2){

  grouping_var <- dplyr::enquo(grouping_var)

  all_nums <- summarize_all_numeric(df = df, num_vars = num_vars, grouping_var = !!grouping_var, num_display = num_display,
                                    show_pval = show_pval, digits = digits)

  all_cats <- summarize_all_categorical(df = df, binary_cat_vars = binary_cat_vars, multiple_cat_vars = multiple_cat_vars,
                                        grouping_var = !!grouping_var, display = cat_display, show_pval = show_pval, digits = digits)

  subs <- summarize_all_subgroups(df = df, subgroups_m = subgroups_m, mean_vars = mean_vars_for_subgroups,
                                  grouping_var = !!grouping_var, num_display = num_display, show_pval = show_pval, digits = digits)


  tb1 <- rbind(all_nums, all_cats, subs)

  if (is.null(order_of_vars)){


    if (export_rtf == TRUE & !is.null(rtf_filename)){

      rtf_table1 <- rtf::RTF(rtf_filename)
      rtf::addTable(rtf_table1, tb1)
      rtf::done(rtf_table1)

      invisible(tb1)

    } else{

      invisible(tb1)

    }

  } else {

    new_table <- tibble::tibble()
    var_order <- order_of_vars

    for(i in 1:length(var_order)){
      for (j in 1:nrow(tb1)){
        if (stringr::str_split(tb1[j, 'var'], " ")[[1]][1] == var_order[i]){
          new_table <- rbind(new_table, tb1[j, ])
          while (!any(stringr::str_split(tb1[j + 1, 'var'], " ")[[1]][1] %in% var_order) & j < nrow(tb1)){
            new_table <- rbind(new_table, tb1[j + 1, ])
            j <- j + 1
          }
        }
      }
    }

    tb1_sorted <- tibble::remove_rownames(new_table)

    if (export_rtf == TRUE & !is.null(rtf_filename)){

      rtf_table1 <- rtf::RTF(rtf_filename)
      rtf::addTable(rtf_table1, tb1_sorted)
      rtf::done(rtf_table1)

      invisible(tb1_sorted)
    } else{

      invisible(tb1_sorted)

    }
  }

}

