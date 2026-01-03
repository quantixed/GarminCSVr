#' Preprocess Garmin activity data
#'
#' Ensures that subsequent functions can be run on Garmin activity data.
#'
#' @param df_all data frame with all Garmin activity data
#' @param from character string of form "YYYY-MM-DD"
#' @param to character string of form "YYYY-MM-DD"
#' @param km numeric target distance in km for the period from from to to
#'
#' @returns checked list of variables: from, to, km
#' @keywords internal

preprocess_garmin_data <- function(df_all = NULL,
                                   from = "", to = "", km = NULL) {

  # check we have a data frame to work with
  if(is.null(df_all)){
    stop("No data frame provided to process_garmin_data().
         Please provide a data frame with Garmin activity data.")
  }

  Date <- NULL

  if(from == "" | to == ""){
    # Date column is in POSIXct format, find min and max, and
    # convert to YYYY-MM-DD string
    if(from == "") {
      from = as.Date(min(df_all$Date))
    }
    if(to == "") {
      to = as.Date(max(df_all$Date))
    }
  }
  # check that from is before to
  if(as.Date(from) > as.Date(to)){
    stop("Invalid date range: 'from' date is after 'to' date.")
  }
  # check that there is data in the date range
  df_subset <- subset(df_all,
                      as.Date(df_all$Date) >= as.Date(from) &
                      as.Date(df_all$Date) <= as.Date(to))
  if(nrow(df_subset) == 0){
    stop(paste0("No activity data found between ", from, " and ", to,
                ". Please check the date range."))
  }
  # if no target distance provided, calculate total distance
  if(is.null(km)) {
    km = sum(subset(df_all,
                    as.Date(df_all$Date) >= as.Date(from) &
                      as.Date(df_all$Date) <= as.Date(to))$Distance)
  }

  # check that there is a valid directory Output/Plots and create if not
  if(!dir.exists("Output/Plots")){
    dir.create("Output/Plots", recursive = TRUE)
  }

  return(list(
    from = from,
    to = to,
    km = km
  ))
}
