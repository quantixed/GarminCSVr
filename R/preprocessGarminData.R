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
