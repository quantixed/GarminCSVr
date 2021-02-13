require(ggplot2)
require(hms)

## Setup preferred directory structure in wd
ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")
ifelse(!dir.exists("Output"), dir.create("Output"), "Folder exists already")
ifelse(!dir.exists("Output/Data"), dir.create("Output/Data"), "Folder exists already")
ifelse(!dir.exists("Output/Plots"), dir.create("Output/Plots"), "Folder exists already")
ifelse(!dir.exists("Script"), dir.create("Script"), "Folder exists already")

## functions

compare2target <- function(activity,fromStr,toStr,df) {
  # filter for activity
  df_window <- subset(df,grepl(tolower(activity),tolower(df_all$Activity.Type)))
  # activities within the window
  fromDate <- as.Date(fromStr)
  toDate <- as.Date(toStr)
  df_window <- subset(df_window, as.Date(df_window$Date) >= fromDate & as.Date(df_window$Date) <= toDate)
  # put them in order
  df_window <- df_window[order(as.Date(df_window$Date)),]
  df_window$Cumulative.Distance <- cumsum(df_window$Distance)
  
  return(df_window)
}

maketarget <- function(fromStr,toStr,km) {
  temp <- seq(as.Date(fromStr), as.Date(toStr), by="days")
  cumdist <- seq(km / length(temp), km, by = km / length(temp))
  df <- data.frame(Date = as.POSIXct(temp),
                   Cumulative.Distance = cumdist)

  return(df)
}

## main script

all_files <- list.files("Data", pattern = "*.csv", full.names = TRUE)
df_all <- read.csv(all_files[1], header = TRUE, stringsAsFactors=FALSE)
for (filename in all_files[-1]) {
    df_temp <- read.csv(filename, stringsAsFactors=FALSE)
    df_all <- rbind(df_all, df_temp)
}
# remove duplicates
df_all <- df_all[!duplicated(df_all), ]
# format Date column to POSIXct
df_all$Date <- as.POSIXct(strptime(df_all$Date, format = "%Y-%m-%d %H:%M:%S"))
df_all <- compare2target("running","2021-01-01","2021-03-31",df_all)
df_target <- maketarget("2021-01-01","2021-03-31", 505)
# wrangle data frames to have matching date columns and then merge, then find difference
# between the cumulative distance and the target
df_temp <- df_all
df_temp$Date <- as.Date(df_all$Date)
df_temp2 <- df_target
df_temp2$Date <- as.Date(df_target$Date)
df_merge <- merge(x = df_temp,
                  y = df_temp2,
                  by = "Date",
                  all.x = TRUE)
df_merge$Difference <- df_merge$Cumulative.Distance.x - df_merge$Cumulative.Distance.y

# plot out cumulative distance over time compared to target
p1 <- ggplot(data = df_all, aes(x = Date, y = Cumulative.Distance)) + 
  geom_line(colour = "blue", size = 1.2) +
  geom_line(data = df_target, linetype = 2) +
  labs(x = "Date", y = "Cumulative Distance (km)")
p1
# plot out how it's going wrt to target
p2 <- ggplot(data = df_merge, aes(x = Date, y = Difference)) + 
  geom_line(colour = "blue", size = 1.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  ylim(-max(abs(df_merge$Difference)),max(abs(df_merge$Difference))) +
  labs(x = "Date", y = "Difference (km)")
p2

# save all plots
ggsave("Output/Plots/progress.png", plot = p1, width = 8, height = 4, dpi = "print")
ggsave("Output/Plots/difference.png", plot = p2, width = 8, height = 4, dpi = "print")
