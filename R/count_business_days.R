# Generate Sequence of Dates
#' @importFrom lubridate %m+%
seq_days_in_months <- function(year, from_month, to_month){
  month_length <- to_month - from_month + 1
  start_date <- as.Date(paste(year, from_month, "01", sep="-"))
  end_date <- start_date %m+% months(month_length) - 1
  dates <- seq(from = start_date, to = end_date, by = 1)
  return(dates)
}

#' Calculating the Number of Business Days
#'
#' @param year target year
#' @param from_month start of the range
#' @param to_month end of the range
#'
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom lubridate wday
#' @importFrom zipangu jholiday
#' @importFrom rlang .data
#' @export
#'
count_business_days <- function(year, from_month, to_month){
  date_array <- seq_days_in_months(year, from_month, to_month)
  data.frame(target_date = date_array,
             day_of_week = lubridate::wday(date_array)) %>%
    dplyr::filter(! .data$target_date %in% zipangu::jholiday(year),
                  ! .data$day_of_week %in% c(1, 7) ) %>%
    nrow()
}
