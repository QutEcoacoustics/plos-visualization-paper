# Title:  Dot-matrix plots - Big Data Visual Analytics paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y., Towsey, M., & Roe, P. (2017, 6-10 November). Visualization
# of environmental audio using ribbon plots and acoustic state sequences. 
# Paper presented at the IEEE International Symposium on Big Data Visual 
# Analytics (BDVA), Adelaide, Australia.

# Description:  This code generates individual dot-matrix plots, one 
# for each day of the recording. This code agglomerates the clusters
# into the seven acoustic classes before representing these classes.

# File (cluster list and sunrise times) and folder requirements
# C:/plos-visualization-paper/data/cluster_list.RData
# C:/plos-visualization-paper/data/Geoscience_Australia_Sunrise_times_Gympie_2015_2016.RData

# Time required: about 6 hours for 398 x 2 sites, this can be 
# interupted at anytime

# Package requirements: NILL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Dot Matrix Plots ------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())
star_time <- paste(Sys.time())

#load(url("https://data.researchdatafinder.qut.edu.au/dataset/cluster-list--"))
load("C:/plos-visualization-paper/data/cluster_list.RData")

# set the start date in "YYYY-MM-DD" format
start_date <- "2015-06-22"

# Generate a date sequence & locate the first of each month
days <- floor(length(cluster_list)/(2*1440))
start <- as.POSIXct(start_date)
interval <- 1440
end <- start + as.difftime(days, units="days")
dates <- seq(from=start, by=interval*60, to=end)
first_of_month <- which(substr(dates, 9, 10)=="01")

# Load the civil dawn, civil dusk and sunrise and sunset times
# Based on Geoscience Australia material
#load(url("https://data.researchdatafinder.qut.edu.au/dataset/sunrise-and-sunset"))
load(file = "C:/plos-visualization-paper/data/Geoscience_Australia_Sunrise_times_Gympie_2015_2016.RData")

a <- which(civil_dawn$dates==paste(substr(start, 1,4), substr(start, 6,7),
                                   substr(start, 9,20),sep = "-"))
reference <- a:(a+days-1)
civil_dawn_times <- civil_dawn$CivSunrise[reference]
civil_dusk_times <- civil_dawn$CivSunset[reference]
sunrise_times <- civil_dawn$Sunrise[reference]
sunset_times <- civil_dawn$Sunset[reference]

# find the minute of civil dawn for each day
civ_dawn <- NULL
for(i in 1:length(civil_dawn_times)) {
  hour <- as.numeric(substr(civil_dawn_times[i], 1,1))
  min <- as.numeric(substr(civil_dawn_times[i], 2,3))
  minute <- hour*60 + min
  civ_dawn <- c(civ_dawn, minute)
}

civ_dusk <- NULL
for(i in 1:length(civil_dusk_times)) {
  hour <- as.numeric(substr(civil_dusk_times[i], 1,2)) 
  min <- as.numeric(substr(civil_dusk_times[i], 3,4))
  minute <- hour*60 + min
  civ_dusk <- c(civ_dusk, minute)
}

sunrise <- NULL
for(i in 1:length(sunrise_times)) {
  hour <- as.numeric(substr(sunrise_times[i], 1,1))
  min <- as.numeric(substr(sunrise_times[i], 2,3))
  minute <- hour*60 + min
  sunrise <- c(sunrise, minute)
}

sunset <- NULL
for(i in 1:length(sunset_times)) {
  hour <- as.numeric(substr(sunset_times[i], 1,2)) 
  min <- as.numeric(substr(sunset_times[i], 3,4))
  minute <- hour*60 + min
  sunset <- c(sunset, minute)
}

site1 <- rep("GympieNP", length(cluster_list)/2)
site2 <- rep("WoondumNP", length(cluster_list)/2)
site <- c(site1, site2)
rm(site1, site2)

# generate a sequence of dates
start <-  strptime("20150622", format="%Y%m%d")
finish <-  strptime("20160723", format="%Y%m%d")
dates <- seq(start, finish, by = "1440 mins")
#any(is.na(dates)) #FALSE
date.list <- NULL
for (i in 1:length(dates)) {
  dat <- substr(as.character(dates[i]),1,10)
  date.list <- c(date.list, dat)
}

# Convert dates to YYYYMMDD format
for (i in 1:length(dates)) {
  x <- "-"
  date.list[i] <- gsub(x, "",date.list[i])  
}
dates <- date.list
rm(date.list)
# duplicate dates 1440 times
dates <- rep(dates, each = 1440)
dates <- rep(dates, 2)
# Add site and dates columns to dataframe
cluster_list <- cbind(cluster_list, site, dates)

# determine the number of days in each month at each site
days_per_month <- NULL
cluster_list <- data.frame(cluster_list)
year_month <- unique(substr(cluster_list$dates,1,6))
for(i in 1:length(year_month)) {
  count <- which(substr(cluster_list$dates, 1, 6)==year_month[i])
  count <- length(count)/1440
  days_per_month <- c(days_per_month, count/2)
}
days_per_period <- rep(days_per_month, each =12)
days_per_period <- rep(days_per_period, 2)

# define cluster classes 
rain <- c(2,10,17,18,21,54,59,60) 
wind <- c(9,19,20,24,25,30,40,42,45,46,47,51,52,56)
birds <- c(3,11,14,15,28,33,37,39,43,57,58)
insects <- c(1,4,22,26,27,29)
cicada <- c(7,8,12,16,32,34,44,48)
planes <- c(49,23)
quiet <- c(5,6,13,31,35,36,38,41,50,53,55)
na <- 61

# colours for each class
insect_col <- "#F0E442"
rain_col <- "#0072B2"
wind_col <- "#56B4E9"
bird_col <- "#009E73"
cicada_col <- "#E69F00"
quiet_col <- "#999999"
plane_col <- "#CC79A7"
na_col <- "white"

feature_colours <- NULL
feature_colours$Feature <- c("INSECTS", "BIRDS", "WIND",
                             "RAIN", "CICADAS", "QUIET",
                             "PLANE", "NA")
feature_colours$colour <- c(insect_col, bird_col, wind_col,
                            rain_col, cicada_col, quiet_col,
                            plane_col, na_col)

cluster_list$col <- "--"
cluster_list$class <- "--"
a <- which(cluster_list$cluster_list %in% insects)
cluster_list$col[a] <- insect_col  
cluster_list$class[a] <- "INSECTS"
a <- which(cluster_list$cluster_list %in% rain)
cluster_list$col[a] <- rain_col
cluster_list$class[a] <- "RAIN"
a <- which(cluster_list$cluster_list %in% wind)
cluster_list$col[a] <- wind_col
cluster_list$class[a] <- "WIND"
a <- which(cluster_list$cluster_list %in% birds)
cluster_list$col[a] <- bird_col
cluster_list$class[a] <- "BIRDS"
a <- which(cluster_list$cluster_list %in% cicada)
cluster_list$col[a] <- cicada_col
cluster_list$class[a] <- "CICADAS"
a <- which(cluster_list$cluster_list %in% quiet)
cluster_list$col[a] <- quiet_col
cluster_list$class[a] <- "QUIET"
a <- which(cluster_list$cluster_list %in% planes)
cluster_list$col[a] <- plane_col
cluster_list$class[a] <- "PLANES"
a <- which(is.na(cluster_list$cluster_list))
cluster_list$col[a] <- na_col
cluster_list$class[a] <- "NA"

a <- which(cluster_list$site=="GympieNP")
gym_cluster_list <- cluster_list[a,]
a <- which(cluster_list$site=="WoondumNP")
woon_cluster_list <- cluster_list[a,]

# This plot takes 3-4 minutes to generate
dates1 <- unique(dates)

sites <- c("GympieNP", "WoondumNP")
for(s in 1:length(sites)) {
  site <- sites[s]
  if(site=="GympieNP") {
    file <- "gym_cluster_list"  
  }
  if(site=="WoondumNP") {
    file <- "woon_cluster_list"  
  }

  for(l in 1:length(dates1)) {
    tiff(paste("C:/plos-visualization-paper/plots/daily_dot_matrix_",site,"_", dates1[l], 
               ".png", sep = ""), width = 1000, height = 1000,
         res=300)
    df <- get(file)[(1440*l-1440+1):(l*1440),]
    par(mar=c(1, 1.1, 0.4, 0.05))
    # Plot an empty plot with no axes or frame
    plot(c(0, 1440), c(1440, 0), 
         type = "n", axes=FALSE, frame.plot=FALSE,
         xlab="", ylab="")
    for(i in 1:length(feature_colours$Feature)) {
      feature <- as.character(feature_colours$Feature[i])
      a <- which(df$class==feature)
      for(j in a) {
        for(k in a) {
          col_code <- feature_colours$colour[i]
          polygon(x = c(k-1, k-1, k, k), 
                  y = c(j-1, j, j, j-1),
                  col=col_code, border = NA)
        }
      }
    }
    # plot title
    if(site=="GympieNP") 
      title(paste("Gympie National Park", dates1[l]), cex.main=0.7, 
            line = -0.2)
    if(site=="WoondumNP") 
      title(paste("Woondum National Park", dates1[l]), cex.main=0.7, 
            line = -0.2)
    # x axis labels
    at <- seq(120, 1440, by = 120)
    axis(side = 1, at = at, line = -2.55, 
         labels = c("2","4","6","8","10","12","14","16","18","20",
                    "22", "24"), tcl=-0.3, 
         cex.axis=0.6, outer = TRUE,
         las=T, pos = NA, tick=FALSE)
    axis(side = 1, at = at, line = -1.53, 
         labels = c("","","","","","","","","","",
                    "", ""), tcl=-0.3, 
         cex.axis=0.6, outer = TRUE,
         las=T, pos = NA, tick=TRUE, lwd=0.2)
    at <- seq(60, 1440, by = 60)
    axis(side = 1, at = at, line = -1.53, 
         labels = c("", "", "", "", "", "",
                    "", "", "", "", "", "", 
                    "", "", "", "", "", "",
                    "", "", "", "", "", ""), tcl=-0.3, 
         cex.axis=0.6, outer = TRUE,
         las=T, pos = NA, tick=TRUE, lwd=0.2)
    # x axis ticks
    at <- seq(0, 1440, by = 15)
    axis(1, line = -1.53, at = at, tick = TRUE,
         labels = FALSE, outer=TRUE, lwd = 0.1, tcl=-0.2)
    
    # vertical and horizontal dotted lines
    x <- c(seq(120, 1440, 120))
    y <- c(seq(121, 1441, 120))
    
    segments(x0 = x, y0 = 0, x1 = x, y1 = 1440,
             lty = 2, lwd = 0.1)
    segments(x0 = 0, y0 = y, x1 = 1441, y1 = y,
             lty = 2, lwd = 0.1)
    x <- c(sunrise[l], sunset[l])
    y <- c(civ_dawn[l], civ_dusk[l])
    segments(x0 = x, y0 = 0, x1 = x, y1 = 1440,
             lty = 2, lwd = 0.9, col="black")
    segments(x0 = 0, y0 = y, x1 = 1441, y1 = y,
             lty = 2, lwd = 0.9, col="black")
    segments(x0 = y, y0 = 0, x1 = y, y1 = 1440,
             lty = 2, lwd = 0.9, col="black")
    segments(x0 = 0, y0 = x, x1 = 1441, y1 = x,
             lty = 2, lwd = 0.9, col="black")
    # y axis labels
    at <- seq(1, 1441, by = 120)
    axis(side=2, at = at, line = -2.23, 
         labels = c("0","2","4","6","8","10",
                    "12","14","16","18","20",
                    "22", "24"), tcl=-0.3,
         cex.axis=0.6, outer = TRUE, 
         las=T, tick=F)
    axis(side=2, at = at, line = -1.63, 
         labels = c("","","","","","",
                    "","","","","",
                    "", ""), tcl=-0.3, lwd=0.2,
         cex.axis=0.6, outer = TRUE, 
         las=T, tick=T)
    at <- seq(1, 1441, by = 60)
    axis(side=2, at = at, line = -1.63, 
         labels = c("", "", "", "", "", "",
                    "", "", "", "", "", "", 
                    "", "", "", "", "", "",
                    "", "", "", "", "", "", ""), 
         tcl=-0.3, lwd=0.2,
         cex.axis=0.6, outer = TRUE, 
         las=T, tick=T)
    # y axis ticks
    at <- seq(1, 1441, by = 15)
    axis(2, line = -1.63, at = at, tick = TRUE,
         labels = FALSE, outer=TRUE, pos=NA, 
         lwd = 0.1, tcl=-0.2)
    mtext(side=1,line=0.1, "24 hours", cex=0.6)
    mtext(side=2,line=0.5, "24 hours", cex=0.6)
    dev.off()
    print(l) # of 398 days x 2 sites
    print(paste(Sys.time()))
  }
}
end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime
