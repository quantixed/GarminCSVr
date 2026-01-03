#' Comparison of yearly Garmin Data
#'
#' @param activity character string of activity type to use as a filter
#' @param datadir character string of folder name where CSV files are stored
#' @param from optional character string in "YYYY-MM-DD" format to filter start date
#' @param to optional character string in "YYYY-MM-DD" format to filter end date
#' @param minimal logical indicating whether to load minimal columns (default FALSE)
#'
#' @returns ggplots comparing years
#' @export
#'
#' @import ggplot2
#' @import dplyr

compare_years <- function(activity = "Running", datadir = "Data",
                          from = "", to = "",
                          minimal = FALSE) {
  Distance <- doy <- Avg.Pace <- Avg.HR <- Year <- NULL
  df_all <- load_garmin_data(activity = activity, datadir = datadir,
                             minimal = minimal)
  if(from != "") {
    from_date <- as.POSIXct(strptime(from, format = "%Y-%m-%d"))
    df_all <- df_all[df_all$Date >= from_date, ]
  }
  if(to != "") {
    to_date <- as.POSIXct(strptime(to, format = "%Y-%m-%d"))
    df_all <- df_all[df_all$Date <= to_date, ]
  }

  # find the year for each entry
  df_all$Year <- format(df_all$Date, "%Y")

  # ggplot of the total distance per year
  p1 <- ggplot(df_all, aes(x = Year, y = Distance)) +
    geom_bar(stat = "summary", fun = "sum", fill = "#cb2027") +
    labs(title = "Total Distance per Year",
         x = "Year",
         y = "Total Distance (km)") +
    theme_bw()
  print(p1)

  # Plot the annual cumulative distance, overlaid
  # Find New Year's Day for each and then work out how many days have elapsed since
  df_all$nyd <- paste(df_all$Year,"-01-01",sep = "")
  df_all$doy <- as.Date(df_all$Date, format="%Y-%m-%d") - as.Date(as.character(df_all$nyd), format="%Y-%m-%d")
  # Calculate cumulative distance for each year
  df_all <- df_all %>%
    group_by(Year) %>%
    arrange(Date) %>%
    mutate(cumsum = cumsum(Distance)) %>%
    ungroup()
  # Make the plot
  p2 <- ggplot(data = df_all, aes(x = doy, y = cumsum, group = Year, color = Year)) +
    geom_line() +
    scale_x_continuous() +
    labs(x = "Days", y = "Cumulative distance (km)") +
    theme_bw()
  print(p2)

  # check that Output/Plots directory exists, if not create it
  if(!dir.exists("Output/Plots")) {
    dir.create("Output/Plots", recursive = TRUE)
  }
  # save plots
  ggsave(filename = "Output/Plots/Yearly_Total_Distance.png",
         plot = p1, width = 8, height = 6)
  ggsave(filename = "Output/Plots/Yearly_Cumulative_Distance.png",
         plot = p2, width = 8, height = 6)

  # if minimal is FALSE then also look at pace vs heart rate
  if(minimal) {
    message("Minimal data loaded, skipping Avg.Pace vs Avg.HR plot.")
    return(invisible(NULL))
  }
  df_all$Avg.Pace <- as.POSIXct(strptime(df_all$Avg.Pace, format = "%M:%S"))
  df_all$ShortLong <- as.factor(ifelse(df_all$Distance > 10, "Long", "Short"))
  # remove any rows with missing Avg.Pace or Avg.HR
  df_all <- df_all[!is.na(df_all$Avg.Pace) & !is.na(df_all$Avg.HR), ]
  # check if we have any rows
  if(nrow(df_all) == 0) {
    stop("No data available for plotting Avg.Pace vs Avg.HR after filtering.")
  }
  # time limits for x-axis
  timelimits <- as.POSIXct(strptime(c("03:30","06:30"), format = "%M:%S"))

  p3 <- ggplot(df_all, aes(x = Avg.Pace, y = Avg.HR)) +
    geom_point(alpha = 0.25, shape = 16) +
    facet_grid(ShortLong ~ Year) +
    scale_x_datetime(limits = timelimits, date_breaks = "1 min",
                     date_labels = "%M:%S") +
    scale_y_continuous(limits = c(120, 180)) +
    labs(x = "Average Pace",
         y = "Average Heart Rate") +
    theme_bw()
  print(p3)

  # save plot
  ggsave(filename = "Output/Plots/Avg_Pace_vs_Avg_HR.png",
         plot = p3, width = 10, height = 8)
}
