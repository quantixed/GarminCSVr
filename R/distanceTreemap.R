#' Distance Treemap
#'
#' Make a treemap of run distances
#'
#' @param df_all data frame with all Garmin activity data
#' @param from string of form "YYYY-MM-DD"
#' @param to string of form "YYYY-MM-DD"
#'
#' @import ggplot2
#' @import dplyr
#' @import grDevices
#' @import treemap
#'
#' @export

distanceTreemap <- function(df_all, from, to) {
  Distance <- NULL
  # load the data (output from process_data() within a timeframe of interest)
  all_data <- get_data_subset(df_all, from, to)
  # max run distance to the next multiple of five
  max_dist <- ceiling(max(all_data$Distance) / 5) * 5
  # make histogram of running distances
  p1 <- ggplot(all_data, aes(x = Distance)) +
    geom_histogram(breaks = seq(from = 0, to = max_dist, by = 1)) +
    labs(x = "Distance (km)", y = "Runs") +
    theme_bw()
  p2 <- ggplot(all_data, aes(x = Distance, weight = Distance)) +
    geom_histogram(breaks = seq(from = 0, to = max_dist, by = 1)) +
    labs(x = "Distance (km)", y = "Total (km)") +
    theme_bw()

  tframe <- paste0(from, "_", to)
  ggsave(paste0("Output/Plots/distanceHist_", tframe, ".png"), p1)
  ggsave(paste0("Output/Plots/distanceHist_w_", tframe, ".png"), p2)

  # bin the data at 5 km and 1 km resolution
  all_data <- all_data %>%
    mutate(
      km5 = cut(Distance, breaks = seq(from = 0, to = max_dist, by = 5)),
      km1 = cut(Distance, breaks = seq(from = 0, to = max_dist, by = 1))
    )

  # rename the categories to give nice labels
  all_data$labelkm5 <- rename_km5(all_data$km5)
  all_data$labelkm1 <- rename_km1(all_data$km1)

  # PNG device
  png(paste0("Output/Plots/treemap", tframe, ".png"), width = 800, height = 800)
  treemap(all_data,
    index = c("labelkm5", "labelkm1"),
    vSize = "Distance",
    type = "index",
    align.labels = list(
      c("left", "top"),
      c("center", "center")
    ),
    palette = "Pastel1",
    overlap.labels = 1,
    title = ""
  )
  dev.off()
}




#' Rename km5
#'
#' Utility function to rename km5 bins for treemap plotting
#'
#' @param x vector with 5 km distance bins
#'
#' @returns character vector with renamed bins
#' @keywords internal
#'
rename_km5 <- function(x) {
  x <- sub("\\(", "", x)
  x <- sub("\\]", " km", x)
  x <- sub(",", " - ", x)
  return(x)
}

#' Rename km1
#'
#' Utility function to rename km1 bins for treemap plotting
#'
#' @param x vector with 1 km distance bins
#'
#' @returns character vector with renamed bins
#' @keywords internal
#'
rename_km1 <- function(x) {
  x <- sub("\\(", "", x)
  x <- sub(",[[:digit:]]+\\]", "", x)
  return(x)
}
