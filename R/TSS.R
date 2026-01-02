#' Function to find and plot Form (CTL, ATL, TSS)
#'
#' @param df_all data frame with all Garmin activity data
#' @param from character string of form "YYYY-MM-DD"
#' @param to character string of form "YYYY-MM-DD"
#'
#' @import lubridate
#'
#' @returns saves plot to Output/Plots folder
#' @export

find_form <- function(df_all, from, to) {
  CTL <- ATL <- TSS <- NULL
  xstart <- xend <- ystart <- yend <- NULL

  # from and to are of the form "YYYY-MM-DD", we need to use a from date that
  # is 90 days earlier so that we get accurate CTL/ATL/TSS values at the start
  # of the window of interest

  # store the true "from" date
  actual_from <- from
  # calculate new from date
  from_date <- as.Date(from) - 90
  from <- format(from_date, "%Y-%m-%d")

  #  and calculate load for each activity
  mydata <- get_data_subset(df_all, from, to)
  # add a column that contains the load of each activity
  # one way to calculate load is to
  # multiply time in hours by avg HR and add 2.5 times avg HR
  # this relates to load by y = ax + b of a = 0.418, b = -150
  mydata$load <- 0.418 * (
    (as.numeric(hms(mydata$Time)) / 3600 * mydata$Avg.HR) +
      (2.5 * mydata$Avg.HR)) - 150
  # make a data frame that has every day in our time window represented
  tl <- makeDateDF(from, to)
  # sum the load for each day
  df <- sumDays(mydata, tl)
  # calculate training loads
  df <- calculateTL(df)

  # data frame for Form zones
  rects <- data.frame(
    ystart = c(20, 5, -10, -30, -50),
    yend = c(30, 20, 5, -10, -30),
    xstart = rep(as.Date(actual_from), 5),
    xend = rep(as.Date(to), 5),
    col = factor(c("Transition", "Fresh", "Grey zone", "Optimal", "High risk"),
                 levels = c(
                   "Transition", "Fresh", "Grey zone", "Optimal", "High risk"))
  )

  # first plot = Fitness and Fatigure
  p1 <- ggplot(df, aes(x = Date)) +
    geom_area(aes(y = CTL), fill = "#58abdf", alpha = 0.2) +
    geom_line(aes(y = CTL), colour = "#58abdf") +
    geom_line(aes(y = ATL), colour = "#5e3cc4") +
    annotate("text", x = as.Date(to),
             y = 0, vjust = "inward", hjust = "inward",
             label = "Fitness", color = "#58abdf") +
    annotate("text", x = as.Date(actual_from),
             y = max(df$ATL), vjust = "inward",
             hjust = "inward", label = "Fatigue", color = "#5e3cc4") +
    labs(x = "", y = "Training load per day") +
    lims(x = c(as.Date(actual_from), as.Date(to))) +
    theme_bw() +
    theme(legend.position = "none")

  # second plot = Form
  p2 <- ggplot(df, aes(x = Date, y = TSS)) +
    geom_line(colour = "#0a0a0a", ) +
    geom_rect(data = rects, inherit.aes = FALSE,
              aes(xmin = xstart, xmax = xend, ymin = ystart, ymax = yend,
                  fill = col), alpha = 0.2) +
    scale_fill_manual(values =
                        c("#DDB140", "#58ABDF", "#A3A3A3",
                          "#67C75D", "#CB2A1D")) +
    labs(x = "", y = "Form") +
    lims(x = c(as.Date(actual_from), as.Date(to))) +
    theme_bw() +
    theme(legend.title = element_blank())

  # patchwork assembly
  p3 <- p1 / p2
  # save plots
  tframe <- paste0(actual_from, "_", to)
  ggsave(paste0("Output/Plots/tss_", tframe, ".png"),
         plot = p3, width = 8, height = 4, dpi = "print")
}


#' Utility function to make date data frame
#'
#' @param fromStr string of form "YYYY-MM-DD"
#' @param toStr string of form "YYYY-MM-DD"
#'
#' @returns data frame with Date, ATL, CTL columns
#' @keywords internal

makeDateDF <- function(fromStr, toStr) {
  temp <- seq(as.Date(fromStr), as.Date(toStr), by = "days")
  df <- data.frame(
    Date = temp,
    ATL = rep(0, length(temp)),
    CTL = rep(0, length(temp))
  )

  return(df)
}


#' Utility function to sum load per day
#'
#' @param df data frame with Date and load columns
#' @param daydf data frame with all dates in the time window
#'
#' @returns data frame with summed load per day
#' @keywords internal

sumDays <- function(df, daydf) {
  df$Date <- as.Date(df$Date)
  tempdf <- aggregate(load ~ Date, data = df, sum)
  newdf <- merge(daydf, tempdf, all.x = TRUE)
  newdf[is.na(newdf)] <- 0

  return(newdf)
}


#' Utility function to calculate training loads
#'
#' The function calculates Acute Training Load (ATL), Chronic Training
#' Load (CTL), and Training Stress Score (TSS) based on the provided load data.
#'
#' @param df data frame with Date and load columns
#'
#' @returns data frame with Date, ATL, CTL, TSS columns
#' @export

calculateTL <- function(df) {
  for (i in 1:nrow(df)) {
    # add today's load to training load(s)
    df$ATL[i] <- df$ATL[i] + df$load[i]
    df$CTL[i] <- df$CTL[i] + df$load[i]
    for (j in (i + 1):(i + 42)) {
      if (j > nrow(df)) {
        break
      }
      df$ATL[j] <- df$ATL[i] * exp(-(j - i) / 7)
      df$CTL[j] <- df$CTL[i] * exp(-(j - i) / 42)
    }
  }
  df <- df[, 1:3]
  df[2] <- df[2] / 7
  df[3] <- df[3] / 42
  df$TSS <- df$CTL - df$ATL

  return(df)
}
