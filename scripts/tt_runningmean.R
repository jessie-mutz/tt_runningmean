# tt_runningmean.R
## script to calculate running mean of temperature for trillium trail common
## garden analysis

## load libraries


## load in data
tt_ws_data <- read.csv('../data/pgh_weatherstation/USW00014762.csv', stringsAsFactors = FALSE)
head(tt_ws_data)

## create moving average function
moving_average_10 <- function(x, n = 10){filter(x, rep(1 / n, n), sides = 1)} # 10-day moving average function

## convert dates to day, month, year, doy
tt_ws_data$r_date <- strptime(tt_ws_data$DATE, '%m/%d/%Y') #JM changed %y to %Y because year includes century (YYYY instead of YY)
tt_ws_data$month <- as.numeric(format(tt_ws_data$r_date, "%m"))
tt_ws_data$day <- as.numeric(format(tt_ws_data$r_date, "%d"))
tt_ws_data$year <- as.numeric(format(tt_ws_data$r_date, "%Y"))
tt_ws_data$doy <- as.numeric(format(tt_ws_data$r_date, "%j"))

## subset out years of interest
tt_ws_data_20162021 <- subset(tt_ws_data, year >2015 & year < 2022)

## caclulate average temperatures
tt_ws_data_20162021$average_temperature <- (rowMeans(tt_ws_data_20162021[,c('TMIN','TMAX')], 
                                                    na.rm = T))/10 # divide by 10 ti get correct T
hist(tt_ws_data_20162021$average_temperature)

## calculate running means
tt_ws_data_20162021$tgrowth10 <- moving_average_10(tt_ws_data_20162021$average_temperature) #JM changed moving_average to moving_average_10 to make function name

## output data
write.csv(tt_ws_data_20162021[,c(1:5,13, 15, 55:61)], 
          '../output/tt_runningmean_tgrowth10.csv', row.names = F)



