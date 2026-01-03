#' Load all Garmin activity data from CSV files
#'
#' This function reads all Garmin activity CSV files from the specified data
#' directory.
#'
#' @param activity character string of activity type to use as a filter
#' @param datadir character string of folder name where CSV files are stored
#' @param minimal logical indicating whether to load minimal columns (default TRUE)
#'
#' @returns data frame of activity data
#' @export

load_garmin_data <- function(activity = "Running", datadir = "Data", minimal = TRUE) {

  Activity.Type <- Title <- Distance <- Time <- NULL
  Cumulative.Distance <- Difference <- Week <- Total.Distance <- Avg.HR <- NULL

  # read all CSV files in datadir folder
  all_files <- list.files(datadir, pattern = "\\.csv$", full.names = TRUE)
  # check that we have a list of CSV files and exit if not
  if (length(all_files) == 0) {
    stop(paste0("No CSV files found in folder '", datadir,
                "'. Please download from Garmin Connect and place them there."))
  }
  # read all CSV files into a single data frame
  # and handle files with differing columns
  # this is the minimal set of data that we need (HR is only needed for TSS)
  if(minimal) {
    cols_needed <- c(
      "Activity.Type", "Date", "Title", "Distance", "Time", "Avg.HR")
    dfs <- lapply(all_files, function(f) {
      df <- read.csv(f, stringsAsFactors = FALSE)
      missing <- setdiff(cols_needed, names(df))
      if (length(missing) > 0) df[missing] <- NA
      df[cols_needed]
    })
    # combine all data frames
    df_all <- do.call(rbind, dfs)
  } else {
    dfs <- lapply(all_files, function(f) read.csv(f, stringsAsFactors = FALSE))
    all_cols <- unique(unlist(lapply(dfs, names)))
    dfs_fixed <- lapply(dfs, function(df) {
      missing <- setdiff(all_cols, names(df))
      if (length(missing)) df[missing] <- NA
      df[all_cols]
    })
    # combine all data frames
    df_all <- do.call(rbind, dfs_fixed)
  }

  # remove duplicates
  df_all <- df_all[!duplicated(df_all), ]
  # we need to be more stringent in the case of duplicates with different
  # columns or name changes etc.
  df_all <- df_all[!duplicated(df_all$Date), ]
  # format Date column to POSIXct
  df_all$Date <- as.POSIXct(strptime(df_all$Date, format = "%Y-%m-%d %H:%M:%S"))
  # sort by Date
  df_all <- df_all[order(df_all$Date), ]
  # ensure Avg.HR is numeric
  df_all$Avg.HR <- as.numeric(df_all$Avg.HR)
  # replace NA with average of Avg.HR
  avg_hr <- mean(df_all$Avg.HR, na.rm = TRUE)
  df_all$Avg.HR[is.na(df_all$Avg.HR)] <- avg_hr
  # finally filter by activity type
  df <- df_all[grep(activity, df_all$Activity.Type, ignore.case = TRUE), ]

  return(df_all)
}
