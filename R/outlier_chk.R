


#' Create indicator variable flagging if a parameter is impacted by a specified condition and change variables that meet condition to NA
#'
#' A condition has been specified and if that specific condition is met then all variables
#' specified in the variable list will be flagged as outliers and their values will be set
#' to missing.
#' Requres tidyverse package
#'
#'
#' @param data Data frame in which to apply the function - should be already in the R environment
#' @param var_list A character list vector of variable names to change to missing/flag if a condition is met (must be contained in the
#'                 dataset entered for 'data')
#' @param condition A logical condition used to define if the variables in var_list should be missing/flagged.
#'                  This should be written as an expression referring to a variable within the dataset.
#'
#'
#' @returns A new data frame containing all variables from the input dataset plus the new flag variables and missing values if condition is met
#' @export
#'
#' @examples
#' iris_sub <- outlier_chk(
#'    data = iris,
#'    var_list = c("Petal.Length", "Petal.Width"),
#'    condition = Petal.Length > 4
#' )
#'



outlier_chk <- function(data, var_list, condition) {

  pacman::p_load(tidyverse, glue, haven, labelled, tidylog, zoo, writexl, broom, splines, purrr, ggplot2, rlang)

  data_sub <- data %>%
    mutate(

      # create flag variables for impacted variables in var_list
      across(all_of(var_list), ~ if_else({{condition}}, 1, 0), .names = "{.col}_outlier"),

      # set values of var_list to missing if condition met
      across(all_of(var_list), ~ if_else({{condition}}, NA, .x))
    )

  return(data_sub)
}























