
#' Creates a sequence of dates
#'
#' @param startDate the begining date of the period
#' @param endDate the end date of the period
#' @param labelDate the label to be used for the date column
#' @param timeInterval the interval of the date sequence
#'
#' @return a data.frame with one date column
#' @export
#'
#' @details The default interval is 1 day. The dates need to be well formatted.
#'   The default title for the date column is "Visit_Date". This function can be
#'   useful in creating timeseries.
dates.create <- function(startDate, endDate=Sys.Date(),
                         labelDate="Visit_Date", timeInterval="1 day"){
  dates <- data.frame(X = as.Date(seq.Date(
    as.Date(startDate), as.Date(endDate), by=timeInterval)))
  colnames(dates) <- NULL
  colnames(dates) <- c(labelDate)

  return(dates)
}
