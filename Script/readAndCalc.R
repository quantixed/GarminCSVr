## The aim of this script is to load and process CSV data from the Garmin Connect website.
## It will check whether you are on track to meet a running goal, i.e. n km between date1 and date2.
## This script will load all csv files in Data/ (in current wd) and filter for Running (and Treadmill Running)
## Place one or moe Garmin CSV outputs into the Data folder for inclusion. Dates for activities can be overlapping
## duplicates are dealt with, so you can just keep adding csvs with the latest data and use the script again.


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
  df_window <- subset(df,grepl(tolower(activity),tolower(df$Activity.Type)))
  # activities within the window
  fromDate <- as.Date(fromStr)
  toDate <- as.Date(toStr)
  df_window <- subset(df_window, as.Date(df_window$Date) >= fromDate & as.Date(df_window$Date) <= toDate)
  # put them in order
  df_window <- df_window[order(as.numeric(df_window$Date)),]
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

process_data <- function(activityStr,fromStr,toStr,km) {
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
  df_all <- compare2target(activityStr,fromStr,toStr,df_all)
  df_target <- maketarget(fromStr,toStr,km)
  # wrangle data frames to have matching date columns and then merge, then find difference
  # between the cumulative distance and the target
  df_temp <- data.frame(Date = as.Date(df_all$Date),
                        Cumulative.Distance = df_all$Cumulative.Distance)
  # add today as final row if we are within the window
  if(as.Date(toStr) >= Sys.Date()) {
    df_temp[nrow(df_temp) + 1,1] = Sys.Date()
    df_temp[nrow(df_temp),2] = df_temp[nrow(df_temp) - 1,2]
  }
  df_temp2 <- df_target
  df_temp2$Date <- as.Date(df_target$Date)
  df_merge <- merge(x = df_temp,
                    y = df_temp2,
                    by = "Date",
                    all.x = TRUE)
  df_merge$Difference <- df_merge$Cumulative.Distance.x - df_merge$Cumulative.Distance.y
  
  # make dataframe to show more granular "balance" of km
  df_debit <- data.frame(Date = df_target$Date,
                         Distance = -(km / nrow(df_target)))
  df_credit <- data.frame(Date = df_all$Date,
                          Distance = df_all$Distance)
  df_balance <- rbind(df_debit,df_credit)
  df_balance <- df_balance[order(as.numeric(df_balance$Date)),]
  df_balance <- subset(df_balance, as.Date(df_balance$Date) <= as.Date(Sys.Date()))
  df_balance$Cumulative.Distance <- cumsum(df_balance$Distance)
  
  # save data
  write.table(df_all, file = "Output/Data/alldata.txt", sep="\t", row.names=FALSE, col.name=TRUE)
  write.table(df_merge, file = "Output/Data/mergedata.txt", sep="\t", row.names=FALSE, col.name=TRUE)
  write.table(df_target, file = "Output/Data/targetdata.txt", sep="\t", row.names=FALSE, col.name=TRUE)
  
  # plot out cumulative distance over time compared to target
  p1 <- ggplot(data = df_all, aes(x = Date, y = Cumulative.Distance)) + 
    geom_line(colour = "blue", size = 1) +
    geom_line(data = df_target, linetype = 2) +
    labs(x = "Date", y = "Cumulative Distance (km)")
  # plot out how it's going wrt to target
  p2 <- ggplot(data = df_merge, aes(x = Date, y = Difference)) + 
    geom_line(colour = "blue", size = 1) +
    geom_hline(yintercept = 0, linetype = 2) +
    ylim(-max(abs(df_merge$Difference)),max(abs(df_merge$Difference))) +
    labs(x = "Date", y = "Difference (km)")
  # more accurate "balance" graph
  p3 <- ggplot(data = df_balance, aes(x = Date, y = Cumulative.Distance)) + 
    geom_line(colour = "blue", size = 1) +
    geom_hline(yintercept = 0, linetype = 2) +
    ylim(-max(abs(df_balance$Cumulative.Distance)),max(abs(df_balance$Cumulative.Distance))) +
    labs(x = "Date", y = "Balance (km)")
  
  # save all plots
  ggsave("Output/Plots/progress.png", plot = p1, width = 8, height = 4, dpi = "print")
  ggsave("Output/Plots/difference.png", plot = p2, width = 8, height = 4, dpi = "print")
  ggsave("Output/Plots/balance.png", plot = p3, width = 8, height = 4, dpi = "print")
  
  # report distances
  print(paste0("Last ",activityStr," activity on: ", toString(df_all[nrow(df_all),2]),". Today is ", toString(Sys.Date())))
  print(paste0("Total ",activityStr," distance between ", fromStr," and ",toStr, " is ", toString(df_merge[nrow(df_merge),2])," km. Goal is ", km, " km."))
  if(df_merge[nrow(df_merge),4] < 0) {
    print(paste0(toString(df_merge[nrow(df_merge),4] * -1)," km behind target."))
  } else {
    print(paste0(toString(df_merge[nrow(df_merge),4])," km ahead of target."))
  }
  print(paste0(toString(km - df_merge[nrow(df_merge),2]), " km to go!"))
}

# to process the data
# Garmin 2021 Running - Stage 1
process_data("running","2021-01-01","2021-03-31",505)
