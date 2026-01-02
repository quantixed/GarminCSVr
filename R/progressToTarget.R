#' Document progress towards target of Garmin activity data
#'
#' @param activity Type of activity (e.g., "Running") - only used for printing
#' @param df_all data frame with all Garmin activity data
#' @param from character string of form "YYYY-MM-DD"
#' @param to character string of form "YYYY-MM-DD"
#' @param km numeric target distance in km for the period from from to to
#'
#' @importFrom utils write.table
#'
#' @returns saves processed data and plots to Output folder
#' @export

progress_to_target <- function(activity = "Running", df_all = NULL,
                         from = "", to = "", km = NULL) {

  Activity.Type <- Title <- Distance <- Time <- NULL
  Cumulative.Distance <- Difference <- Week <- Total.Distance <- Avg.HR <- NULL

  # data in the window (sorted by date) with cumulative distance
  df_all <- get_data_subset(df_all, from, to)

  # get weekly summary data frame (used for plotting later)
  weekly_summary_df <- weekly_summary(df_all, from, to)

  # add monthly summary here?

  # make target data frame that is a linear progression in the window towards
  # the target km goal
  df_target <- make_linear_target(from, to, km)
  # wrangle data frames to have matching date columns and then merge,
  # then find difference between the cumulative distance and the target
  df_temp <- data.frame(
    Date = as.Date(df_all$Date),
    Cumulative.Distance = df_all$Cumulative.Distance
  )
  # add today as final row if we are within the window
  if (as.Date(to) >= Sys.Date()) {
    df_temp[nrow(df_temp) + 1, 1] <- Sys.Date()
    df_temp[nrow(df_temp), 2] <- df_temp[nrow(df_temp) - 1, 2]
  }
  df_temp2 <- df_target
  df_temp2$Date <- as.Date(df_target$Date)
  df_merge <- merge(
    x = df_temp,
    y = df_temp2,
    by = "Date",
    all.x = TRUE
  )
  df_merge$Difference <-
    df_merge$Cumulative.Distance.x - df_merge$Cumulative.Distance.y

  # make dataframe to show more granular "balance" of km
  df_debit <- data.frame(
    Date = df_target$Date,
    Distance = -(km / nrow(df_target))
  )
  df_credit <- data.frame(
    Date = df_all$Date,
    Distance = df_all$Distance
  )
  df_balance <- rbind(df_debit, df_credit)
  df_balance <- df_balance[order(as.numeric(df_balance$Date)), ]
  df_balance <- subset(df_balance,
                       as.Date(df_balance$Date) <= as.Date(Sys.Date()))
  df_balance$Cumulative.Distance <- cumsum(df_balance$Distance)
  df_balance$Date <- as.Date(df_balance$Date, format = "%Y-%m-%d %H:%M:%S")

  # plot out cumulative distance over time compared to target
  p1 <- ggplot(data = df_all, aes(x = Date, y = Cumulative.Distance)) +
    geom_line(colour = "#cb2027", linewidth = 1) +
    geom_line(data = df_target, linetype = 2) +
    geom_abline(slope = 0, intercept = km, linetype = 3, colour = "grey") +
    labs(x = "Date", y = "Cumulative Distance (km)") +
    theme_bw()
  # plot out how it's going wrt to target
  p2 <- ggplot(data = df_merge, aes(x = Date, y = Difference)) +
    geom_line(colour = "#cb2027", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = 2) +
    xlim(as.Date(from), as.Date(to)) +
    ylim(-max(abs(df_merge$Difference)), max(abs(df_merge$Difference))) +
    labs(x = "Date", y = "Difference (km)") +
    theme_bw()
  # more accurate "balance" graph
  p3 <- ggplot(data = df_balance, aes(x = Date, y = Cumulative.Distance)) +
    geom_line(colour = "#cb2027", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = 2) +
    xlim(as.Date(paste0(from, " 00:00:00"),
                 format = "%Y-%m-%d %H:%M:%S"),
         as.Date(paste0(to, " 23:59:59"), format = "%Y-%m-%d %H:%M:%S")) +
    ylim(-max(abs(df_balance$Cumulative.Distance)),
         max(abs(df_balance$Cumulative.Distance))) +
    labs(x = "Date", y = "Balance (km)") +
    theme_bw()
  # weekly plot
  p4 <- ggplot(weekly_summary_df, aes(x = Week, y = Total.Distance)) +
    geom_col(fill = "#cb2027") +
    geom_hline(yintercept = mean(weekly_summary_df$Total.Distance),
               color = "darkgrey", linetype = 2) +
    lims(y = c(0, 2 * mean(weekly_summary_df$Total.Distance))) +
    labs(title = "Weekly Running Summary", x = "Week", y = "Distance (km)") +
    theme_bw()

  tframe <- paste0(from, "_", to)

  # save all plots
  ggsave(paste0("Output/Plots/progress_",
                tframe, ".png"),
         plot = p1, width = 8, height = 4, dpi = "print")
  ggsave(paste0("Output/Plots/difference_",
                tframe, ".png"),
         plot = p2, width = 8, height = 4, dpi = "print")
  ggsave(paste0("Output/Plots/balance_",
                tframe, ".png"),
         plot = p3, width = 8, height = 4, dpi = "print")
  ggsave(paste0("Output/Plots/weekly_summary_",
                tframe, ".png"),
         plot = p4, width = 8, height = 4, dpi = "print")

  # report distances
  cat(paste0("Last ", activity, " activity on: ",
             toString(df_all[nrow(df_all), 2]),
             ". Today is ", toString(Sys.Date()), "\n"))
  cat(paste0("Total ", activity,
             " distance between ", from, " and ", to, " is ",
             toString(df_merge[nrow(df_merge), 2]),
             " km. Goal is ", km, " km.\n"))
  if (df_merge[nrow(df_merge), 4] < 0) {
    cat(paste0(toString(df_merge[nrow(df_merge), 4] * -1),
               " km behind target.\n"))
  } else {
    cat(paste0(toString(df_merge[nrow(df_merge), 4]),
               " km ahead of target.\n"))
  }
  if (km > df_merge[nrow(df_merge), 2]) {
    cat(paste0(toString(km - df_merge[nrow(df_merge), 2]),
               " km to go!\n"))
  } else {
    cat("You did it!\n")
  }
  cat(paste0("In this period you have run ",
             sum(floor(df_all$Distance / 21)), " half-marathons.\n"))
}


#' Function to subset activities within a window and find cumulative distance
#'
#' @param df data frame with all activities
#' @param from character string of form "YYYY-MM-DD"
#' @param to character string of form "YYYY-MM-DD"
#'
#' @returns data frame with activities of type activity within date window
#' @export

get_data_subset <- function(df, from, to) {

  # activities within the window
  fromDate <- as.Date(from)
  toDate <- as.Date(to)
  df <- subset(df, as.Date(df$Date) >= fromDate & as.Date(df$Date) <= toDate)
  # put them in order
  df <- df[order(as.numeric(df$Date)), ]
  df$Cumulative.Distance <- cumsum(df$Distance)

  return(df)
}


#' Function to assemble target cumulative distance data frame
#'
#' @param from character string of form "YYYY-MM-DD"
#' @param to character string of form "YYYY-MM-DD"
#' @param km numeric target distance in km for the period from from to to
#'
#' @returns data frame with Date, Cumulative.Distance columns
#' @keywords internal

make_linear_target <- function(from, to, km) {
  temp <- seq(as.Date(from), as.Date(to), by = "days")
  cumdist <- seq(km / length(temp), km, by = km / length(temp))
  df <- data.frame(
    Date = as.POSIXct(temp),
    Cumulative.Distance = cumdist
  )

  return(df)
}

#' Calculate weekly summary of distances
#'
#' @param df data frame with Date and Distance columns
#' @param from character string of form "YYYY-MM-DD"
#' @param to character string of form "YYYY-MM-DD"
#'
#' @importFrom stats aggregate
#'
#' @returns data frame with weekly total and daily average distances
#' @keywords internal

weekly_summary <- function(df, from, to) {
  df_filtered <- df[df$Date >= as.POSIXct(from) &
    df$Date <= as.POSIXct(to), ]
  df_filtered$Week <- as.Date(cut(
    df_filtered$Date, breaks = "week", start.on.monday = TRUE))
  weekly_summary <- aggregate(Distance ~ Week,
    data = df_filtered,
    FUN = function(x) c(Total = sum(x), Daily.Average = sum(x) / 7)
  ) # do not use mean

  # Flatten the multi-level columns
  weekly_summary <- do.call(data.frame, weekly_summary)
  colnames(weekly_summary) <- c(
    "Week", "Total.Distance", "Daily.Average.Distance")

  return(weekly_summary)
}

