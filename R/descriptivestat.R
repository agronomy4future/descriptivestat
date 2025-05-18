#' Embed Descriptive Statistics into Original Dataset
#'
#' Computes and appends group-level descriptive statistics—such as mean, variance, standard deviation, standard error,
#' confidence intervals, coefficient of variation, and interquartile ranges—to the original dataset. Returns both
#' summarized and observed rows with a clear distinction.
#'
#' @param data A data frame containing the input dataset.
#' @param group_vars Character vector of column names used to group the data (e.g., "treatment", "rep").
#' @param value_vars Character vector of numeric variable names to compute statistics for (e.g., "yield", "biomass").
#' @param output_stats Character vector of statistics to compute. Valid options include:
#' \itemize{
#'   \item \code{"v"}: Variance
#'   \item \code{"sd"}: Standard deviation
#'   \item \code{"se"}: Standard error
#'   \item \code{"ci"}: 95% confidence interval (LCI, UCI)
#'   \item \code{"cv"}: Coefficient of variation
#'   \item \code{"iqr"}: Interquartile range (Q1, Q2, Q3)
#' }
#'
#' @return A data frame that includes both descriptive statistics (tagged as \code{category = "mean"}) and the original
#' observations (tagged as \code{category = "observed"}), along with a \code{group_id} column for easy grouping.
#'
#' @examples
#' \dontrun{
#'
#'if(!require(remotes)) install.packages("remotes")
#'if (!requireNamespace("descriptivestat", quietly = TRUE)) {
#'    remotes::install_github("agronomy4future/descriptivestat", force= TRUE)
#'}
#'library(remotes)
#'library(descriptivestat)
#'
#'df= data.frame(
#'tr1= c("A", "A", "B", "B", "A", "A", "B", "B", "A", "A", "B", "B"),
#'tr2= c("C", "D", "C", "D", "C", "D", "C", "D", "C", "D", "C", "D"),
#'block= c("I", "I", "I", "I", "II", "II", "II", "II", "III", "III", "III", "III"),
#'yield= c(122, 122, 121, 144, 121, 122, 111, 111, 125, 98, 100, 121)
#')
#'
#'descriptivestat(
#'   data= df,
#'   group_vars= c("tr1","tr2"),
#'   value_vars= c("yield"),
#'   output_stats= c("se","cv")
#' )
#' }
#'
#' @import dplyr
#' @import purrr
#' @export
descriptivestat= function(data, group_vars, value_vars, output_stats= c("v", "sd", "se", "ci", "cv", "iqr")) {

  if(!require(dplyr)) install.packages("dplyr")
  library(dplyr)
  if(!require(purrr)) install.packages("purrr")
  library(purrr)

  # Validate and match requested stats
  output_stat= match.arg(output_stats, choices = c("v", "sd", "se", "ci", "cv", "iqr"), several.ok= TRUE)

  # Ensure numeric values
  data[value_vars]= lapply(data[value_vars], function(x) as.numeric(as.character(x)))

  # Compute summaries
  summary_list= lapply(value_vars, function(var) {
    base_summary= data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(!!var:= mean(.data[[var]], na.rm= TRUE), .groups = "drop")

    if ("v" %in% output_stats) {
      base_summary= base_summary %>%
        left_join(
          data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(!!paste0("v.", var):= var(.data[[var]], na.rm= TRUE), .groups = "drop"),
          by = group_vars
        )
    }

    if ("sd" %in% output_stats) {
      base_summary= base_summary %>%
        left_join(
          data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(!!paste0("sd.", var):= sd(.data[[var]], na.rm= TRUE), .groups = "drop"),
          by= group_vars
        )
    }

    if ("se" %in% output_stats) {
      base_summary= base_summary %>%
        left_join(
          data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(!!paste0("se.", var):= sd(.data[[var]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[var]]))),
                      .groups = "drop"),
          by= group_vars
        )
    }

    if ("ci" %in% output_stats) {
      base_summary= base_summary %>%
        left_join(
          data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(
              n= sum(!is.na(.data[[var]])),
              se= sd(.data[[var]], na.rm= TRUE) / sqrt(n),
              mean= mean(.data[[var]], na.rm = TRUE),
              !!paste0("LCI.", var):= mean - qt(0.975, df= n - 1) * se,
              !!paste0("UCI.", var):= mean + qt(0.975, df= n - 1) * se,
              .groups = "drop"
            ) %>% select(-n, -se, -mean),
          by= group_vars
        )
    }

    if ("cv" %in% output_stats) {
      base_summary= base_summary %>%
        left_join(
          data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(
              mean= mean(.data[[var]], na.rm= TRUE),
              sd= sd(.data[[var]], na.rm= TRUE),
              !!paste0("cv.", var):= (sd / mean),
              .groups= "drop"
            ) %>% select(-mean, -sd),
          by= group_vars
        )
    }

    if ("iqr" %in% output_stats) {
      base_summary= base_summary %>%
        left_join(
          data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(
              !!paste0("Q1.", var):= quantile(.data[[var]], probs= 0.25, na.rm= TRUE),
              !!paste0("Q2.", var):= quantile(.data[[var]], probs= 0.50, na.rm= TRUE),
              !!paste0("Q3.", var):= quantile(.data[[var]], probs= 0.75, na.rm= TRUE),
              .groups = "drop"
            ),
          by= group_vars
        )
    }

    base_summary
  })

  summary_stats= reduce(summary_list, full_join, by= group_vars) %>%
    mutate(
      category= "mean",
      group_id= apply(select(., all_of(group_vars)), 1, paste, collapse= "_")
    )

  # Add extra columns like block
  extra_cols= setdiff(names(data), c(group_vars, value_vars))
  for (col in extra_cols) {
    if (!col %in% names(summary_stats)) {
      summary_stats[[col]]= NA
    }
  }

  summary_stats= summary_stats %>%
    select(all_of(names(data)), category, group_id, everything())

  # Observed rows
  observed= data %>%
    mutate(
      category= "observed",
      group_id= apply(select(., all_of(group_vars)), 1, paste, collapse= "_")
    )

  # Add missing stat columns to observed
  stat_cols= setdiff(names(summary_stats), names(observed))
  for (col in stat_cols) {
    observed[[col]]= NA
  }

  observed= observed %>% select(names(summary_stats))

  bind_rows(summary_stats, observed) %>%
    arrange(across(all_of(group_vars)), desc(category== "mean"))
}
