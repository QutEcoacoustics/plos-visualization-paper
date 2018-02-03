# Title:  Two-hour Plots - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code contains a function (plot_funct) that
# generates a series of two hour plots. A two hour plot displays
# the average number of minutes each month in each two hour period
# throughout the day. These plots relate to the Woondum National Park data.
# A dataframe (cluster_list) is created numbering the minute of the day,  
# site, date,period of the day and a unique combined reference to the site, 
# year, month and period. This unique reference is used by the barplot 
# function to produce plots for each month contained in the dataset. Note:  
# the average for each month is calculated from the number of days recorded, 
# this may be less than the number of days in the month, and the code 
# accounts for the number of days available.

# File and folder requirements
# These will be loaded automatically
# C:/plos-visualization-paper/data/gympieclusterlist
# C:/plos-visualization-paper/data/woondumclusterlist
# C:/plos-visualization-paper/plots

# Time requirement: less than 30 seconds

# Package requirements: NILL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Two hour plots  --------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls())
start_time <- paste(Sys.time())

# make folder for plots if it does not exist
f <- paste0("C:/plos-visualization-paper/plots/")
if (!dir.exists(f)) {
  dir.create("C:/plos-visualization-paper/plots/")
}

# Load and read the cluster list (if necessary)
u <- "https://data.researchdatafinder.qut.edu.au/dataset/62de1856-d030-423b-9ada-0b16eb06c0ba/resource/7a70163b-323b-4c30-aaf3-e19e934b328d/download/gympieclusterlist.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/",name,sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/", basename(u)))
  rm(f, u)
}
gympie_cluster_list <- read.csv(paste0("C:/plos-visualization-paper/data/",name,sep=""))

u <- "https://data.researchdatafinder.qut.edu.au/dataset/62de1856-d030-423b-9ada-0b16eb06c0ba/resource/2e264574-2c24-45b0-ad98-fc1ca231f0b5/download/woondumclusterlist.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/",name,sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/", basename(u)))
  rm(u)
}
if (file.exists(f)) {
  rm(f, u)
}
woondum_cluster_list <- read.csv(paste0("C:/plos-visualization-paper/data/",name,sep=""))
cluster_list <- c(gympie_cluster_list, woondum_cluster_list)
cluster_list <- c(cluster_list[[1]],cluster_list[[2]])

rm(gympie_cluster_list, woondum_cluster_list, name)

minute_reference <- c(0:1439)
minute_reference <- rep(minute_reference, (length(cluster_list)/1440))

cluster_list <- data.frame(cluster_list)
colnames(cluster_list) <- "cluster_list"
cluster_list$minute_reference <- as.integer(0)
cluster_list$minute_reference <- minute_reference

site1 <- rep("GympieNP", nrow(cluster_list)/2)
site2 <- rep("WoondumNP", nrow(cluster_list)/2)
site <- c(site1, site2)

# generate a sequence of dates
start <-  strptime("20150622", format="%Y%m%d")
finish <- strptime("20160723", format="%Y%m%d")
dates <- seq(start, finish, by = "1440 mins")
any(is.na(dates)) #FALSE
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
year_month <- unique(substr(cluster_list$dates,1,6))
for(i in 1:length(year_month)) {
  count <- which(substr(cluster_list$dates, 1, 6)==year_month[i])
  count <- length(count)/1440
  days_per_month <- c(days_per_month, count/2)
}
days_per_period <- rep(days_per_month, each =12)
days_per_period <- rep(days_per_period, 2)

# Assign time periods to minute of the day
cluster_list$period <- 0
periods <- seq(0, 1440, 120)
for(i in 1:(length(periods)-1)) {
  a <- which(cluster_list$minute_reference > periods[i]-1 
             & cluster_list$minute_reference < periods[i+1])
  if(i < 10) {cluster_list$period[a] <- paste("0", i, sep = "")}
  if(i >= 10) {cluster_list$period[a] <- paste(i, sep = "")}
}

cluster_list$site_yrMth_per <- paste(cluster_list$site, 
                                     substr(cluster_list$dates,1,6),
                                     cluster_list$period, 
                                     sep = "_")

a <- table(cluster_list$cluster_list, cluster_list$site_yrMth_per)

a <- as.data.frame(a)

# replace the NAs with 1000 to track these minutes
NA_ref <- which(is.na(cluster_list$cluster_list))
cluster_list$cluster_list[NA_ref] <- 1000
# Create a 3 column table containing variables and frequencies
a <- table(cluster_list$cluster_list, cluster_list$site_yrMth_per)
a <- as.data.frame(a)

# produce list of when the nas occured
z <- which(a$Var1==1000)
na_reference <- a[z,3]

# colours for each class
insects_col <- "#F0E442"
rain_col <- "#0072B2"
wind_col <- "#56B4E9"
birds_col <- "#009E73"
cicadas_col <- "#E69F00"
quiet_col <- "#999999"
planes_col <- "#CC79A7"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot_funct function ------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot_funct <- function(clust_num, colour, site) {
  y <- which(a$Var1==clust_num)
  cluster_reference <- a[y,3]
  cluster_reference <- cbind(cluster_reference, 
                             days_per_period,
                             na_reference)
  cluster_reference <- as.data.frame(cluster_reference)
  cluster_reference$output <- cluster_reference$cluster_reference/
    (cluster_reference$days_per_period - (cluster_reference$na_reference/120))
  cluster_reference$output <- round(cluster_reference$output,2)
  
  months <- unique(substr(cluster_list$dates,1,6))
  for(i in 1:length(months)) {
    if(substr(months[i],5,6)=="01") {months[i] <- paste("JAN", substr(months[i],3,4), sep=" ")}
    if(substr(months[i],5,6)=="02") {months[i] <- paste("FEB", substr(months[i],3,4), sep=" ")}
    if(substr(months[i],5,6)=="03") {months[i] <- paste("MAR", substr(months[i],3,4), sep=" ")}  
    if(substr(months[i],5,6)=="04") {months[i] <- paste("APR", substr(months[i],3,4), sep=" ")}  
    if(substr(months[i],5,6)=="05") {months[i] <- paste("MAY", substr(months[i],3,4), sep=" ")}
    if(substr(months[i],5,6)=="06") {months[i] <- paste("JUN", substr(months[i],3,4), sep=" ")}
    if(substr(months[i],5,6)=="07") {months[i] <- paste("JUL", substr(months[i],3,4), sep=" ")}  
    if(substr(months[i],5,6)=="08") {months[i] <- paste("AUG", substr(months[i],3,4), sep=" ")}  
    if(substr(months[i],5,6)=="09") {months[i] <- paste("SEP", substr(months[i],3,4), sep=" ")}
    if(substr(months[i],5,6)=="10") {months[i] <- paste("OCT", substr(months[i],3,4), sep=" ")}
    if(substr(months[i],5,6)=="11") {months[i] <- paste("NOV", substr(months[i],3,4), sep=" ")}  
    if(substr(months[i],5,6)=="12") {months[i] <- paste("DEC", substr(months[i],3,4), sep=" ")}  
  }
  started <- seq(1,nrow(cluster_reference),12)
  finished <- started + 11
  
  months <- rep(months, 2)
  
  num_of_plots <- length(started)
  max <- max(cluster_reference$output)
  if(site=="site1") {
    for(i in 1:14) { # num_of_plots should be even
      # The y-axis labels are plotted for the 1st and 15th plots
      num1 <- c(1,15)
      if(i %in% num1) {
        mp <- barplot(cluster_reference$output[started[i]:finished[i]],
                      ylim=c(0, max), col = colour, xlab = "", las=1,
                      mgp = c(1, 0.4, 0), tck = - 0.05)
        spacing <- mp[2] - mp[1]
        # adjust the midpoints (mp)
        mp <- mp - spacing/2
        mp <- c(mp, mp[length(mp)]+ spacing)
        at <- mp    #seq.int(0.4, 14, length.out = 12)
        Axis(side = 1, labels = FALSE, 
             at = at, cex = 0.4, 
             tck = -0.05, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, at = c(at[1], at[7], at[13]),
             cex = 0.4, tck = -0.14, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, at = c(at[4], at[10]),
             cex = 0.4, tck = -0.09, mgp = c(1,0.4,0))
      }
      num2 <- c(2:14,16:28) 
      if(i %in% num2) {
        mp <- barplot(cluster_reference$output[started[i]:finished[i]],
                      ylim=c(0,max), col = colour, xlab = "", axes=FALSE,
                      mgp = c(1,0.5,0), tck = - 0.05)
        spacing <- mp[2] - mp[1]
        # adjust the midpoints (mp)
        mp <- mp - spacing/2
        mp <- c(mp, mp[length(mp)]+ spacing)
        at <- mp    #seq.int(0.4, 14, length.out = 12)
        Axis(side = 2, labels=FALSE, tck = -0.05, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, tck = -0.05,
             at = at, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, tck = -0.14,
             at = c(at[1], at[7], at[13]), mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, at = c(at[4], at[10]),
             cex = 0.4, tck = -0.09, mgp = c(1,0.4,0))
      }
      mtext(side = 3, paste(months[i]), line = 0.3, cex = 0.7)
      mtext(side = 1, text = c(as.character(seq(0,24,12))), 
            at = c(at[1]+0.7, at[7], at[13]-0.7), line=-0.1, cex = 0.5)
    }  
  }
  if(site=="site2") {
    for(i in 15:28) { # num_of_plots should be even
      # The y-axis labels are plotted for the 1st and 15th plots
      num1 <- c(1,15)
      if(i %in% num1) {
        mp <- barplot(cluster_reference$output[started[i]:finished[i]],
                      ylim=c(0, max), col = colour, xlab = "", las=1,
                      mgp = c(1, 0.4, 0), tck = - 0.05)
        spacing <- mp[2] - mp[1]
        # adjust the midpoints (mp)
        mp <- mp - spacing/2
        mp <- c(mp, mp[length(mp)]+ spacing)
        at <- mp
        Axis(side = 1, labels = FALSE,
             at = at, cex = 0.4, tck = -0.05, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE,
             at = c(at[1], at[7], at[13]), cex = 0.4, 
             tck = -0.14, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, at = c(at[4], at[10]),
             cex = 0.4, tck = -0.09, mgp = c(1,0.4,0))
      }
      num2 <- c(2:14,16:28) 
      if(i %in% num2) {
        mp <- barplot(cluster_reference$output[started[i]:finished[i]],
                      ylim=c(0,max), col = colour, xlab = "", axes=FALSE,
                      mgp = c(1,0.5,0), tck = - 0.05)
        spacing <- mp[2] - mp[1]
        # adjust the midpoints (mp)
        mp <- mp - spacing/2
        mp <- c(mp, mp[length(mp)]+ spacing)
        at <- mp
        Axis(side = 2, labels=FALSE, tck = -0.05, mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, tck = -0.05,
             at = mp, mgp = c(1, 0.4, 0))
        Axis(side = 1, labels = FALSE, tck = -0.14,
             at = c(at[1], at[7], at[13]), mgp = c(1,0.4,0))
        Axis(side = 1, labels = FALSE, at = c(at[4], at[10]),
             cex = 0.4, tck = -0.09, mgp = c(1,0.4,0))
      }
      mtext(side = 3, paste(months[i]), line = 0.3, cex = 0.7)
      mtext(side = 1, text = c(as.character(seq(0,24,12))), 
            at = c(at[1]+0.7, at[7], at[13]-0.7), line=-0.1, cex = 0.5)
    }
  }
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Two hour plots for Plos One paper ------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tiff("C:/plos-visualization-paper/plots/Two-hour_plots.tiff",
     width = 2025, height = 1350, units = 'px', res = 300)
par(mfrow=c(4, 14), 
    mar=c(1, 0, 2, 0.1), oma=c(0.9, 2.1, 0, 0), xpd = NA,
    cex = 0.9, cex.axis = 0.54, cex.main = 0.9)

# Start insect image
clust_num <- 29
colour <- insects_col
# Set the site to Woondum (site2)
plot_funct(clust_num, colour, "site2")
mtext(side = 3, line = 1, "a. ORTHOPTERA - Cluster 29                                                                                                                                 ", cex=1.1)

# Start Bird image
clust_num <- 37
colour <- birds_col
# Set the site to Woondum (site2)
plot_funct(clust_num, colour, "site2")
mtext(side = 3, line = 1, "b. BIRDS - Cluster 37                                                                                                                                 ", cex=1.1)

mtext(side = 2, line = 1.3, outer = T, cex = 0.8,
      "Average number of cluster minutes in 2 hour period each month")

# Start cicada image
clust_num <- 48
colour <- cicadas_col
# Set the site to Woondum (site2)
plot_funct(clust_num, colour, "site2")
mtext(side = 3, line = 1, "c. CICADAS - Cluster 48                                                                                                                                 ", cex=1.1)

# Start quiet image
cluster <- 13
colour <- quiet_col
# Set the site to Woondum (site2)
plot_funct(cluster, colour, "site2")
mtext(side = 3, line = 1, "d. QUIET - Cluster 13                                                                                                                                 ", cex=1.1)
mtext(side = 1, "Time (hours)", outer = T)
dev.off()

end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime
# End of code --------------------------------
