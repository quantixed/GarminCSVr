#' Calendar Heatmap
#'
#' Creates a colour-coded calendar to visualise time series data
#'
#' @param dates A vector containing the dates in `Date` format.
#' @param values A vector containing the corresponding values as numeric.
#' @param title Main plot title (optional).
#' @param subtitle Main plot subtitle (optional).
#' @param legendtitle Legend title (optional).
#'
#' @import ggplot2
#' @import lubridate
#' @importFrom utils read.csv
#' @importFrom stats na.omit
#'
#' @return ggplot object
#'
calendarHeatmap <- function(dates,
                            values,
                            title = "",
                            subtitle = "",
                            legendtitle = ""
) {
  dowmapped <- value <- woy <- x <- xend <- y <- yend <- NULL

  # Parameter checks
  if (missing(dates)) {
    stop("Need to specify a dates vector.")
  }
  if (missing(values)) {
    stop("Need to specify a values vector.")
  }
  if (!is.Date(dates)) {
    stop("dates vector need to be in Date format.")
  }
  if (length(dates) != length(values)) {
    stop("dates and values need to have the same length.")
  }


  # Custom theme
  my_theme <- function() {
    # Colors
    color.background <- "white"
    color.text <- "#22211d"

    # Begin construction of chart
    theme_bw(base_size = 15) +

      # Format background colors
      theme(
        panel.background =
          element_rect(fill = color.background, color = color.background)
      ) +
      theme(
        plot.background =
          element_rect(fill = color.background, color = color.background)
      ) +
      theme(
        panel.border =
          element_rect(color = color.background)
      ) +
      theme(
        strip.background =
          element_rect(fill = color.background, color = color.background)
      ) +

      # Format the grid
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks = element_blank()) +

      # Format the legend
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(size = 8, color = color.text)) +
      theme(
        legend.title =
          element_text(size = 10, face = "bold", color = color.text)
      ) +

      # Format title and axis labels
      theme(
        plot.title =
          element_text(color = color.text, size = 20, face = "bold")
      ) +
      theme(
        axis.text.x =
          element_text(size = 12, color = "black")
      ) +
      theme(
        axis.text.y =
          element_text(size = 12, color = "black")
      ) +
      theme(
        axis.title.x =
          element_text(size = 14, color = "black", face = "bold")
      ) +
      theme(
        axis.title.y =
          element_text(size = 14, color = "black", vjust = 1.25)
      ) +
      theme(
        axis.text.x =
          element_text(size = 10, hjust = 0, color = color.text)
      ) +
      theme(
        axis.text.y =
          element_text(size = 10, color = color.text)
      ) +
      theme(
        strip.text =
          element_text(face = "bold")
      ) +

      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }

  # create empty calendar
  min.date <- as.Date(paste(format(min(dates), "%Y"), "-1-1", sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"), "-12-31", sep = ""))
  df <- data.frame(date = seq(min.date, max.date, by = "days"), value = NA)

  # fill in values
  df$value[match(dates, df$date)] <- values

  df$year <- as.factor(format(df$date, "%Y"))
  df$month <- as.numeric(format(df$date, "%m"))
  df$doy <- as.numeric(format(df$date, "%j"))
  df$dow <- as.numeric(format(df$date, "%w"))
  df$woy <- as.numeric(format(df$date, "%U")) + 1

  df$dowmapped <- ordered(df$dow, levels = 6:0)
  levels(df$dowmapped) <- rev(c(
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
  ))

  # colours for the plot
  continuous <- is.numeric(values)

  if (continuous) {
    # for continuous data use custom color scale
    color_scale <- scale_fill_gradientn(
      colours = turbo(255),
      na.value = "white",
      name = legendtitle,
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(75, units = "mm"),
        title.position = "top",
        title.hjust = 0.5
      )
    )
  } else {
    # for count data use discrete color scale
    no_values <- length(unique(na.omit(values)))
    no_values <- turbo(no_values + 2)
    # discard first colour (very dark)
    no_values <- no_values[-1]
    color_scale <- scale_fill_manual(
      values = no_values,
      na.value = "white",
      na.translate = FALSE,
      name = legendtitle,
      guide = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5
      )
    )
  }


  # create plot
  g <- ggplot(df, aes(woy, dowmapped, fill = value)) +
    geom_tile(colour = "darkgrey") +
    facet_wrap(~year, ncol = 1) + # Facet for years
    coord_equal(xlim = c(2.5, 54)) + # square tiles
    scale_x_continuous(
      breaks = 53 / 12 * (1:12) - 1.5,
      labels = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      )
    ) +
    my_theme() +
    color_scale +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle
    )

  my.lines <- data.frame(
    x = numeric(),
    y = numeric(),
    xend = numeric(),
    yend = numeric(),
    year = character()
  )

  for (years in levels(df$year)) {
    df.subset <- df[df$year == years, ]

    y.start <- df.subset$dow[1]
    x.start <- df.subset$woy[1]

    x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5)
    y.top.left <- 7.5
    x.top.right <- df.subset$woy[nrow(df.subset)] + 0.5
    y.top.right <- 7.5

    x.mid.left01 <- x.start - 0.5
    y.mid.left01 <- 7.5 - y.start
    x.mid.left02 <- x.start + 0.5
    y.mid.left02 <- 7.5 - y.start

    x.bottom.left <- x.start - 0.5
    y.bottom.left <- 0.5
    x.bottom.right <- ifelse(y.start == 6,
                             df.subset$woy[nrow(df.subset)] + 0.5,
                             df.subset$woy[nrow(df.subset)] - 0.5
    )
    y.bottom.right <- 0.5

    my.lines <- rbind(
      my.lines,
      data.frame(
        x = c(
          x.top.left, x.bottom.left,
          x.mid.left01, x.top.left, x.bottom.left
        ),
        y = c(
          y.top.left, y.bottom.left,
          y.mid.left01, y.top.left, y.bottom.left
        ),
        xend = c(
          x.top.right, x.bottom.right,
          x.mid.left02, x.mid.left02, x.mid.left01
        ),
        yend = c(
          y.top.right, y.bottom.right,
          y.mid.left02, y.mid.left02, y.mid.left01
        ),
        year = years
      )
    )

    # lines to separate months
    for (j in 1:12) {
      df.subset.month <- max(df.subset$doy[df.subset$month == j])
      x.month <- df.subset$woy[df.subset.month]
      y.month <- df.subset$dow[df.subset.month]

      x.top.mid <- x.month + 0.5
      y.top.mid <- 7.5

      x.mid.mid01 <- x.month - 0.5
      y.mid.mid01 <- 7.5 - y.month - 1
      x.mid.mid02 <- x.month + 0.5
      y.mid.mid02 <- 7.5 - y.month - 1

      x.bottom.mid <- ifelse(y.month == 6, x.month + 0.5, x.month - 0.5)
      y.bottom.mid <- 0.5

      my.lines <- rbind(
        my.lines,
        data.frame(
          x = c(x.top.mid, x.mid.mid01, x.mid.mid01),
          y = c(y.top.mid, y.mid.mid01, y.mid.mid01),
          xend = c(x.mid.mid02, x.mid.mid02, x.bottom.mid),
          yend = c(y.mid.mid02, y.mid.mid02, y.bottom.mid),
          year = years
        )
      )
    }
  }

  # add lines
  g <- g +
    geom_segment(
      data = my.lines, aes(x, y, xend = xend, yend = yend),
      lineend = "square", color = "black", inherit.aes = FALSE
    )

  return(g)
}

#' Calendar View
#'
#' Creates calendar heatmaps for number of runs and distance per day
#'
#' @param df_all Data frame with all activity data
#' @param from Start date in "YYYY-MM-DD" format
#' @param to End date in "YYYY-MM-DD" format
#'
#' @import dplyr
#' @import ggplot2
#' @import patchwork
#' @import timetk
#' @importFrom utils read.csv
#' @export
#'
#' @return Saves calendar heatmaps as single PNG file
#'
calendarView <- function(df_all, from, to) {
  Date <- Distance <- NULL

  # get data subset for the period
  df_day <- get_data_subset(df_all, from, to)
  # plot the data
  df_day$Date <- as.Date(df_day$Date)
  df_day <- df_day %>%
    summarize_by_time(
      .date_var = Date,
      .by = "day",
      Distance = sum(Distance),
      n = as.character(n())
    )
  # from string is a date like "2018-01-01" so get year from it with substr
  yr <- substr(from, 1, 4)
  p1 <- calendarHeatmap(df_day$Date, df_day$n,
                        title = paste("Running", yr), subtitle = "Runs per day"
  )
  p2 <- calendarHeatmap(df_day$Date, df_day$Distance,
                        title = "", subtitle = "km per day"
  )
  # compile plots with patchwork
  p <- p1 / p2
  tframe <- paste0(from, "_", to)
  ggsave(paste0("Output/Plots/calendar_per_day_", tframe, ".png"), p,
         width = 6, height = 8, dpi = "print")
}

#' Create turbo palette (taken from colorTK)
#'
#' Turbo is a jet alternative developed by Anton Mikhailov
#'
#' @param paletteLength.num number of colors to generate.
#' @param space.chr character string; interpolation in RGB or CIE Lab color
#'   spaces. See ?grDevices::colorRamp for more details. (Default "rgb")
#' @param interpolate.chr use spline or linear interpolation. See
#'   ?grDevices::colorRamp for more details. (Default "linear")
#' @param bias.num <numeric>: a positive number.  Higher values give more widely
#'   spaced colors at the high end. See ?grDevices::colorRamp for more details.
#'   (Default 1)
#' @return  A vector of color.
#' @keywords internal

turbo <- function(paletteLength.num = NULL, space.chr = "rgb", interpolate.chr = "linear", bias.num = 1) {
  grDevices::colorRampPalette(
    colors = c(
      "#30123B", "#321543", "#33184A", "#341B51", "#351E58", "#36215F", "#372466",
      "#38276D", "#392A73", "#3A2D79", "#3B2F80", "#3C3286", "#3D358B", "#3E3891",
      "#3F3B97", "#3F3E9C", "#4040A2", "#4143A7", "#4146AC", "#4249B1", "#424BB5",
      "#434EBA", "#4451BF", "#4454C3", "#4456C7", "#4559CB", "#455CCF", "#455ED3",
      "#4661D6", "#4664DA", "#4666DD", "#4669E0", "#466BE3", "#476EE6", "#4771E9",
      "#4773EB", "#4776EE", "#4778F0", "#477BF2", "#467DF4", "#4680F6", "#4682F8",
      "#4685FA", "#4687FB", "#458AFC", "#458CFD", "#448FFE", "#4391FE", "#4294FF",
      "#4196FF", "#4099FF", "#3E9BFE", "#3D9EFE", "#3BA0FD", "#3AA3FC", "#38A5FB",
      "#37A8FA", "#35ABF8", "#33ADF7", "#31AFF5", "#2FB2F4", "#2EB4F2", "#2CB7F0",
      "#2AB9EE", "#28BCEB", "#27BEE9", "#25C0E7", "#23C3E4", "#22C5E2", "#20C7DF",
      "#1FC9DD", "#1ECBDA", "#1CCDD8", "#1BD0D5", "#1AD2D2", "#1AD4D0", "#19D5CD",
      "#18D7CA", "#18D9C8", "#18DBC5", "#18DDC2", "#18DEC0", "#18E0BD", "#19E2BB",
      "#19E3B9", "#1AE4B6", "#1CE6B4", "#1DE7B2", "#1FE9AF", "#20EAAC", "#22EBAA",
      "#25ECA7", "#27EEA4", "#2AEFA1", "#2CF09E", "#2FF19B", "#32F298", "#35F394",
      "#38F491", "#3CF58E", "#3FF68A", "#43F787", "#46F884", "#4AF880", "#4EF97D",
      "#52FA7A", "#55FA76", "#59FB73", "#5DFC6F", "#61FC6C", "#65FD69", "#69FD66",
      "#6DFE62", "#71FE5F", "#75FE5C", "#79FE59", "#7DFF56", "#80FF53", "#84FF51",
      "#88FF4E", "#8BFF4B", "#8FFF49", "#92FF47", "#96FE44", "#99FE42", "#9CFE40",
      "#9FFD3F", "#A1FD3D", "#A4FC3C", "#A7FC3A", "#A9FB39", "#ACFB38", "#AFFA37",
      "#B1F936", "#B4F836", "#B7F735", "#B9F635", "#BCF534", "#BEF434", "#C1F334",
      "#C3F134", "#C6F034", "#C8EF34", "#CBED34", "#CDEC34", "#D0EA34", "#D2E935",
      "#D4E735", "#D7E535", "#D9E436", "#DBE236", "#DDE037", "#DFDF37", "#E1DD37",
      "#E3DB38", "#E5D938", "#E7D739", "#E9D539", "#EBD339", "#ECD13A", "#EECF3A",
      "#EFCD3A", "#F1CB3A", "#F2C93A", "#F4C73A", "#F5C53A", "#F6C33A", "#F7C13A",
      "#F8BE39", "#F9BC39", "#FABA39", "#FBB838", "#FBB637", "#FCB336", "#FCB136",
      "#FDAE35", "#FDAC34", "#FEA933", "#FEA732", "#FEA431", "#FEA130", "#FE9E2F",
      "#FE9B2D", "#FE992C", "#FE962B", "#FE932A", "#FE9029", "#FD8D27", "#FD8A26",
      "#FC8725", "#FC8423", "#FB8122", "#FB7E21", "#FA7B1F", "#F9781E", "#F9751D",
      "#F8721C", "#F76F1A", "#F66C19", "#F56918", "#F46617", "#F36315", "#F26014",
      "#F15D13", "#F05B12", "#EF5811", "#ED5510", "#EC530F", "#EB500E", "#EA4E0D",
      "#E84B0C", "#E7490C", "#E5470B", "#E4450A", "#E2430A", "#E14109", "#DF3F08",
      "#DD3D08", "#DC3B07", "#DA3907", "#D83706", "#D63506", "#D43305", "#D23105",
      "#D02F05", "#CE2D04", "#CC2B04", "#CA2A04", "#C82803", "#C52603", "#C32503",
      "#C12302", "#BE2102", "#BC2002", "#B91E02", "#B71D02", "#B41B01", "#B21A01",
      "#AF1801", "#AC1701", "#A91601", "#A71401", "#A41301", "#A11201", "#9E1001",
      "#9B0F01", "#980E01", "#950D01", "#920B01", "#8E0A01", "#8B0902", "#880802",
      "#850702", "#810602", "#7E0502", "#7A0403"
    ),
    space = space.chr,
    interpolate = interpolate.chr,
    bias = bias.num
  )(paletteLength.num)
}
