require(ggplot2)
require(dplyr)
require(hms)
file_name <- file.choose()
df1 <- read.csv(file_name, header = TRUE, stringsAsFactors = FALSE)
# format Date column to POSIXct
df1$Date <- as.POSIXct(strptime(df1$Date, format = "%Y-%m-%d %H:%M:%S"))
# format Avg.Pace to POSIXct
df1$Avg.Pace <- as.POSIXct(strptime(df1$Avg.Pace, format = "%M:%S"))
# make groups of different distances using ifelse
df1$Type <- ifelse(df1$Distance < 5, "< 5 km", ifelse(df1$Distance < 8, "5-8 km", ifelse(df1$Distance < 15, "8-15 km", ">15 km")))
# make factors for these so that they're in the right order when we make the plot
df1$Type_f = factor(df1$Type, levels=c("< 5 km","5-8 km","8-15 km", ">15 km"))
# plot out average pace over time
p1 <- ggplot( data = df1, aes(x = Date,y = Avg.Pace, color = Distance)) + 
  geom_point() +
  scale_y_datetime(date_labels = "%M:%S") +
  geom_smooth(color = "orange") +
  labs(x = "Date", y = "Average Pace (min/km)")
p1
# plot out same data grouped by distance
p2 <- ggplot( data = df1, aes(x = Date,y = Avg.Pace, group = Type_f, color = Type_f)) + 
  geom_point() +
  scale_y_datetime(date_labels = "%M:%S") +
  geom_smooth() +
  labs(x = "Date", y = "Average Pace (min/km)", colour = NULL) +
  facet_grid(~Type_f)
p2
# now look at stride length. first remove zeros
df1[df1 == 0] <- NA
# now find earliest valid date
date_v <- df1$Date
# change dates to NA where there is no avg stride data
date_v <- as.Date.POSIXct(ifelse(df1$Avg.Stride.Length > 0, df1$Date, NA))
# find min and max for x-axis
earliest_date <- min(date_v, na.rm = TRUE)
latest_date <- max(date_v, na.rm = TRUE)
# make the plot
p3 <- ggplot(data = df1, aes(x = Date,y = Avg.Stride.Length, group = Type_f, color = Type_f)) +
  geom_point() + 
  ylim(0, NA) + xlim(as.POSIXct(earliest_date), as.POSIXct(latest_date)) +
  geom_smooth() +
  labs(x = "Date", y = "Average stride length (m)", colour = NULL) +
  facet_grid(~Type_f)
p3
df1$Avg.HR <- as.numeric(as.character(df1$Avg.HR))
p4 <- ggplot(data = df1, aes(x = Date,y = Avg.HR, group = Type_f, color = Type_f)) +
  geom_point() +
  ylim(0, NA) + xlim(as.POSIXct(earliest_date), as.POSIXct(latest_date)) +
  geom_smooth() +
  labs(x = "Date", y = "Average heart rate (bpm)", colour = NULL) +
  facet_grid(~Type_f)
p4
# plot out average pace per distance coloured by year
p5 <- ggplot( data = df1, aes(x = Distance,y = Avg.Pace, color = Date)) + 
  geom_point() +
  scale_y_datetime(date_labels = "%M:%S") +
  geom_smooth(color = "orange") +
  labs(x = "Distance (km)", y = "Average Pace (min/km)")
p5
# make a date factor for year to group the plots
df1$Year <- format(as.Date(df1$Date, format="%d/%m/%Y"),"%Y")
p6 <- ggplot( data = df1, aes(x = Distance,y = Avg.Pace, group = Year, color = Year)) + 
  geom_point() +
  scale_y_datetime(date_labels = "%M:%S") +
  geom_smooth() +
  labs(x = "Distance", y = "Average Pace (min/km)") +
  facet_grid(~Year)
p6
# Cumulative sum over years
df1 <- df1[order(as.Date(df1$Date)),]
df1 <- df1 %>% group_by(Year) %>% mutate(cumsum = cumsum(Distance))
p7 <- ggplot( data = df1, aes(x = Date,y = cumsum, group = Year, color = Year)) + 
  geom_line() +
  labs(x = "Date", y = "Cumulative distance (km)")
p7
# Plot these cumulative sums overlaid
# Find New Year's Day for each and then work out how many days have elapsed since
df1$nyd <- paste(df1$Year,"-01-01",sep = "")
df1$Days <- as.Date(df1$Date, format="%Y-%m-%d") - as.Date(as.character(df1$nyd), format="%Y-%m-%d")
# Make the plot
p8 <- ggplot( data = df1, aes(x = Days,y = cumsum, group = Year, color = Year)) + 
  geom_line() +
  scale_x_continuous() +
  labs(x = "Days", y = "Cumulative distance (km)")
p8
# save all plots
ggsave("allPace.png", plot = p1, width = 8, height = 4, dpi = "print")
ggsave("paceByDist.png", plot = p2, width = 8, height = 4, dpi = "print")
ggsave("strideByDist.png", plot = p3, width = 8, height = 4, dpi = "print")
ggsave("HRByDist.png", plot = p4, width = 8, height = 4, dpi = "print")
ggsave("allPaceByDist.png", plot = p5, width = 8, height = 4, dpi = "print")
ggsave("paceByDistByYear.png", plot = p6, width = 8, height = 4, dpi = "print")
ggsave("cumulativeDistByYear.png", plot = p7, width = 8, height = 4, dpi = "print")
ggsave("cumulativeDistOverlay.png", plot = p8, width = 8, height = 4, dpi = "print")

