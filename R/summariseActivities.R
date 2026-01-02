#' Wrapper to analyse and visualise Garmin activities
#'
#' This function loads Garmin activity data, filters it by activity type and date range,
#' and then generates several visualisations and analyses including progress towards a target,
#' Training Stress Score (TSS) form analysis, a distance treemap, and a
#' calendar view of activities.
#'
#' @param activity Type of activity to filter (e.g., "Running") must be contained
#' in the 'Activity.Type' column of the Garmin data, e.g. Running will include
#' all runs, including treadmill runs.
#' @param datadir directory where Garmin CSV files are stored (default is "Data")
#' @param from start date in "YYYY-MM-DD" format
#' @param to end date in "YYYY-MM-DD" format
#' @param target optional numeric target distance in km for the period from
#' 'from' to 'to'
#' @param progress logical indicating whether to generate progress towards target plot
#' @param form logical indicating whether to generate TSS form analysis plot
#' @param tree logical indicating whether to generate distance treemap plot
#' @param calendar logical indicating whether to generate calendar view plot
#'
#' @returns NULL
#' @export

summarise_activities <- function(activity = "Running",
                                 datadir = "Data",
                                 from = "",
                                 to = "",
                                 target = NULL,
                                 progress = TRUE,
                                 form = TRUE,
                                 tree = TRUE,
                                 calendar = TRUE
                                 ){
  # load all data into a data frame
  df <- load_garmin_data(activity = activity, datadir = datadir)

  # visualise progress during the goal, towards an optional target
  checked <- preprocess_garmin_data(df, from = from, to = to, km = target)
  from <- checked$from
  to <- checked$to
  target <- checked$km
  cat(paste0("Processing ", activity,
               " activities from ", from, " to ", to, ". With a target of ",
               target, " km\n"))
  if(progress) {
    progress_to_target(activity = activity, df, from = from, to = to, km = target)
  }

  # TSS score
  if(form) {
    find_form(df, from = from, to = to)
  }

  # distance map
  if(tree) {
    distanceTreemap(df, from = from, to = to)
  }

  # calendar view
  if(calendar) {
    calendarView(df, from = from, to = to)
  }
}
