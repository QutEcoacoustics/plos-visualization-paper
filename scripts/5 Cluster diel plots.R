# Title:  Cluster Diel Plots - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code generates two cluster diel plots on for each 
# recording site using the input of a cluster list, the cluster colours
# and the sunrise and sunset times and saves them into the plots folder.

# File and requirements: 
# C:/plos-visualization-paper/data/cluster_list.RData
# C:/plos-visualization-paper/data/civil_dawn_2015_2016.RData
# C:/plos-visualization-paper/plots

# Time requirements: about 3 mintues

# Package requirements
# nil

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cluster Diel Plot code ------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Yvonne F. Phillips
# Description:  This code produces a simulated dot-matrix plot from a 100 day acoustic state sequence, see Chapter 11.
# remove all objects in the global environment
rm(list = ls())

# set the start date in "YYYY-MM-DD" format
start_date <- "2015-06-22"

# load cluster list
load(file="C:/plos-visualization-paper/data/cluster_list.RData")

# Generate a date sequence & locate the first of each month
days <- floor(length(cluster_list)/(2*1440))
start <- as.POSIXct(start_date)
interval <- 1440
end <- start + as.difftime(days, units="days")
dates <- seq(from=start, by=interval*60, to=end)
first_of_month <- which(substr(dates, 9, 10)=="01")

# Load the civil dawn, civil dusk and sunrise and sunset times
load(file="C:/plos-visualization-paper/data/civil_dawn_2015_2016.RData")

# convert the start date in "YYYY-MM-DD" format
start <- as.POSIXct(start_date)

# find the start date in the civil_dawn dataset
a <- which(civil_dawn$dates==paste(substr(start, 1,4), substr(start, 6,7),
                                   substr(start, 9,20),sep = "-"))
reference <- a:(a+days-1)
civil_dawn_times <- civil_dawn$CivSunrise[reference]
civil_dusk_times <- civil_dawn$CivSunset[reference]
sunrise_times <- civil_dawn$Sunrise[reference]
sunset_times <- civil_dawn$Sunset[reference]

# prepare the civil dawn times
civ_dawn <- NULL
for(i in 1:length(civil_dawn_times)) {
  hour <- as.numeric(substr(civil_dawn_times[i], 1,1))
  min <- as.numeric(substr(civil_dawn_times[i], 2,3))
  minute <- hour*60 + min
  civ_dawn <- c(civ_dawn, minute)
}

# prepare the civil dusk times
civ_dusk <- NULL
for(i in 1:length(civil_dusk_times)) {
  hour <- as.numeric(substr(civil_dusk_times[i], 1,2)) 
  min <- as.numeric(substr(civil_dusk_times[i], 3,4))
  minute <- hour*60 + min
  civ_dusk <- c(civ_dusk, minute)
}

# prepare the sunrise times
sunrise <- NULL
for(i in 1:length(sunrise_times)) {
  hour <- as.numeric(substr(sunrise_times[i], 1,1))
  min <- as.numeric(substr(sunrise_times[i], 2,3))
  minute <- hour*60 + min
  sunrise <- c(sunrise, minute)
}

# prepare the sunset times
sunset <- NULL
for(i in 1:length(sunset_times)) {
  hour <- as.numeric(substr(sunset_times[i], 1,2)) 
  min <- as.numeric(substr(sunset_times[i], 3,4))
  minute <- hour*60 + min
  sunset <- c(sunset, minute)
}

site <- c(rep("Site 1", length(cluster_list)/2),
          rep("Site 2", length(cluster_list)/2))

# generate a sequence of dates, these must match the number of days
days <- length(cluster_list)/(1440*2)
finish <- start + as.difftime((days-1), units="days")
dates <- seq(start, finish, by = "1440 mins")
# duplicate dates 1440 times
dates <- rep(rep(dates, each = (1440)), 2)

# Add site and dates columns to dataframe
cluster_list <- data.frame(cluster_list)
cluster_list <- cbind(cluster_list, site, dates)

# determine the number of days in each month at each site
days_per_month <- NULL
year_month <- unique(substr(cluster_list$dates,1,7))
for(i in 1: length(year_month)) {
  count <- which(substr(cluster_list$dates,1,7)==year_month[i])
  count <- length(count)/1440
  days_per_month <- c(days_per_month, count/2)
}

# define acoustic classes 
rain <- c(2, 10, 17, 18, 21, 54, 59, 60) 
wind <- c(9, 19, 20, 24, 25, 30, 40, 42, 45, 46, 47, 51, 52, 56)
birds <- c(3, 11, 14, 15, 28 ,33, 37, 39, 43, 57, 58) 
insects <- c(1, 4, 22, 26, 27, 29)
cicada <- c(7, 8, 12, 16, 32, 34, 44, 48)
planes <- c(49, 23)
quiet <- c(5, 6, 13, 31, 35, 36, 38, 41, 50, 53, 55)

# colours for each class
insect_col <- "#F0E442"
rain_col <- "#0072B2"
wind_col <- "#56B4E9"
bird_col <- "#009E73"
cicada_col <- "#E69F00"
quiet_col <- "#999999"
plane_col <- "#CC79A7"
na_col <- "white"

cluster_list$col <- "colour"
a <- which(cluster_list$cluster_list %in% rain)
cluster_list$col[a] <- rain_col
a <- which(cluster_list$cluster_list %in% wind)
cluster_list$col[a] <- wind_col
a <- which(cluster_list$cluster_list %in% birds)
cluster_list$col[a] <- bird_col
a <- which(cluster_list$cluster_list %in% insects)
cluster_list$col[a] <- insect_col
a <- which(cluster_list$cluster_list %in% cicada)
cluster_list$col[a] <- cicada_col
a <- which(cluster_list$cluster_list %in% quiet)
cluster_list$col[a] <- quiet_col
a <- which(cluster_list$cluster_list %in% planes)
cluster_list$col[a] <- plane_col
a <- which(is.na(cluster_list$cluster_list))
cluster_list$col[a] <- na_col

cols <- cluster_list$col

site1 <- cluster_list[1:(nrow(cluster_list)/2),]
site2 <- cluster_list[(1+(nrow(cluster_list)/2)):nrow(cluster_list),]
rm(cluster_list)

for(i in 1:2) {
  k <- i
  if(k==1) {
    site <- "GympieNP"
    cluster_list <- site1
    cols <- cluster_list$col
    tiff(paste("C:/plos-visualization-paper/plots/Cluster_diel_", site[1],
               "_",start,"_to_", finish, ".tiff", sep=""), 
         width = 1713, height = 1300, units = 'px', res = 300)
  }
  if(k==2) {
    site <- "WoondumNP"
    cluster_list <- site2
    cols <- cluster_list$col
    tiff(paste("C:/plos-visualization-paper/plots/Cluster_diel_", site[1],
               "_",start,"_to_", finish, ".tiff", sep=""), 
         width = 1713, height = 1300, units = 'px', res = 300)
  }
  par(mar=c(0.9, 3.1, 0.9, 3.1), mgp = c(3,0.8,0),
      cex = 0.6, cex.axis = 1.2, cex.main = 1)
  
  # Plot an empty plot with no axes or frame
  plot(c(0,1440), c(days,1), type = "n", axes=FALSE, 
       frame.plot=FALSE,
       xlab="", ylab="") #, asp = 398/1440)
  # Create the heading
  mtext(side=3, line = -1, cex = 0.8,
        paste("Cluster diel plot - ", site," ", format(dates[1], "%d %B %Y")," - ", 
              format(dates[length(dates)-1], "%d %B %Y"), 
              sep=""))
  ref <- 0
  # draw coloured polygons row by row
  # set the rows starting at the top of the plot
  for(j in (days):1) {
    # set the column starting on the left
    for(k in 1:1440) {
      ref <- ref[1] + 1
      # draw a square for each minute in each day 
      # using the polygon function mapping the cluster
      # number to a colour
      cluster <- cluster_list$cluster_list[ref]
      col_ref <- cols[ref] # problem when it gets to 9286
      polygon(c(k,k,k+1,k+1), c(j,(j-1),(j-1),j),
              col=col_ref,
              border = NA)
    }
  }
  # draw horizontal lines
  first_of_each_month <- days - first_of_month + 1
  if(length(first_of_month) >= 1){
    for(i in 1:length(first_of_month)) {
      lines(c(1,1441), c(first_of_each_month[i], 
                         first_of_each_month[i]), 
            lwd=1, lty = 3)
    }
  }
  
  # draw vertical lines
  at <- seq(0,1440, 240) + 1
  for(i in 1:length(at)) {
    lines(c(at[i], at[i]), c(1,days), lwd=1, lty=3)
  }
  # label the x axis
  axis(1, tick = FALSE, at = at, 
       labels = c("12 am","4 am",
                  "8 am","12","4 pm",
                  "8 pm","12 pm"), line = -1.4)
  # plot the left axes
  axis(side = 2, at = first_of_each_month, tick = FALSE, 
       labels=format(dates[first_of_month],"%b %Y"), 
       las=1, line = -2.4, cex.axis=1, hadj=1.2)
  # plot the right axes
  axis(side = 4, at = first_of_each_month, tick = FALSE, 
       labels=format(dates[first_of_month],"%b %Y"), 
       las=1, line = -2.4, cex.axis=1, hadj=-0.16)
  
  at <- seq(0, 1440, 240)
  
  # draw dotted line to show civil-dawn
  for(i in length(civ_dawn):1) {
    lines(c(civ_dawn), c(length(civ_dawn):1), 
          lwd=1, lty=2, col="black")
  }
  
  # draw dotted line to show civil-dusk
  for(i in length(civ_dusk):1) {
    lines(c(civ_dusk), c(length(civ_dusk):1), 
          lwd=1, lty=2, col="black")
  }
  # draw dotted line to show sunrise
  for(i in length(sunrise):1) {
    lines(c(sunrise), c(length(sunrise):1),  
          lwd=1, lty=2, col="black")
  }
  # draw dotted line to show sunset
  for(i in length(sunset):1) {
    lines(c(sunset), c(length(sunset):1),  
          lwd=1, lty=2, col="black")
  }
  dev.off()
}
