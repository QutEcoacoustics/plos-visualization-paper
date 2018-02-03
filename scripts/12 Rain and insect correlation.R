# Title:  Rain and insect correlation - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code generates a plot that displays the association 
# between the rain and orthopthera clusters 

# Note: This file should only be run after 9 Rose plots.R because it requires 
# the polar_plot.csv file in the results folder

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Rain and insect correlation ---------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())
start_time <- paste(Sys.time())

# read file containing summary of 30 minute segments
df <- read.csv("C:/Work/Projects/Twelve_month_clustering/Saving_dataset/polarHistograms/polar_data.csv", header = T)

# convert from 30 minute to 24 hour summaries
length <- nrow(df)
gym_df <- df[1:(length/2),]
woon_df <- df[(1+length/2):length,]

days <- floor(nrow(gym_df)/(48*60))
clust <- unique(df$score)
ref <- 1440/30 * length(clust)
gym_total <- NULL
for(i in 1:days) {
  gym_df_day <- gym_df[(1+ref*(i-1)):(i*ref),]
  for(j in 1:length(clust)) {
    a <- which(gym_df_day$score==clust[j])
    gym_df_cl <- gym_df_day[a,]
    tot <-sum(gym_df_cl$value)
    gym_total <- c(gym_total, tot)
  }
}

woon_total <- NULL
for(i in 1:days) {
  woon_df_day <- woon_df[(1+ref*(i-1)):(i*ref),]
  for(j in 1:length(clust)) {
    a <- which(woon_df_day$score==clust[j])
    woon_df_cl <- woon_df_day[a,]
    tot <-sum(woon_df_cl$value)
    woon_total <- c(woon_total, tot)
  }
}

gym_matrix <- matrix(gym_total, nrow = days, 
                     ncol = length(clust), byrow = TRUE)
woon_matrix <- matrix(woon_total, nrow = days, 
                      ncol = length(clust), byrow = TRUE)

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
# set all dates to "" except the 1st of each month
a <- which(!substr(date.list, 9, 10)=="01")
date.list[a] <- ""

# colours for each class
insect_col <- "#F0E442"
rain_col <- "#0072B2"
wind_col <- "#56B4E9"
bird_col <- "#009E73"
cicada_col <- "#E69F00"
quiet_col <- "#999999"
plane_col <- "#CC79A7"
na_col <- "white"

a <- which(substr(date.list, 9, 10)=="01")
# repeat date in the next position
date.list[a+1] <- date.list[a]
blank_date_list <- rep("", length(date.list))
#b <- max(n)- a + 1

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# choose the day numbers
n <- 1:101
# choose the rain clusters
rain_clusters <- c(10, 18, 21, 59)
x <- cbind(gym_matrix[n, rain_clusters])

# choose the insect clusters
insect_clusters <- c(1,27,29)
y <- cbind(gym_matrix[n,insect_clusters])

cor(rowSums(x), rowSums(y))
gym_x <- rowSums(x)
gym_y <- rowSums(y)

max_x <- max(gym_x)
max_y <- max(gym_y)


# Woondum
# choose the rain clusters
x <- cbind(woon_matrix[n, rain_clusters])
y <- cbind(woon_matrix[n, insect_clusters])

cor(rowSums(x), rowSums(y))
woon_x <- rowSums(x)
woon_y <- rowSums(y)

max_x <- max(woon_x)
max_y <- max(woon_y)

# Plot an empty plot with no axes or frame
#png(paste("plots\\woondum_Insect",toString(insect_clusters),"_rain",
#          toString(rain_clusters), "_n_", min(n), "_", max(n), ".png", sep = ""), 
#    height = 675, width = 1500)
m <- rbind(c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(2,2,2),
           c(2,2,2))
layout(m)
layout.show(2)

for(i in 1:length(date.list)) {
  if(date.list[i]=="2015-07-01") {
    date.list[i] <- paste(substr(date.list[i],10,10),
                          "Jul",
                          substr(date.list[i],1,4), sep=" ")
  }
  if(date.list[i]=="2015-08-01") {
    date.list[i] <- paste(substr(date.list[i],10,10),
                          "Aug",
                          substr(date.list[i],1,4), sep=" ")
  }
  if(date.list[i]=="2015-09-01") {
    date.list[i] <- paste(substr(date.list[i],10,10),
                          "Sept",
                          substr(date.list[i],1,4), sep=" ")
  }
}

date.list
#par(mar=c(1,3,3,0))
tiff("C:/Work/Projects/Twelve_month_clustering/Saving_dataset/Figures for plos article/Fig15.tiff", 
     width = 2025, height = 1380, units = 'px', res = 300)

par(mar=c(0.1, 3.4, 3.5, 0),  #, mfcol=c(2,1) ,
    cex = 0.6, cex.axis = 1, cex.main = 1)
# set up the plot layout
m <- rbind(c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(1,1,1),
           c(2,2,2),
           c(3,3,3),
           c(3,3,3),
           c(3,3,3),
           c(3,3,3),
           c(3,3,3),
           c(3,3,3))
layout(m)
layout.show(3)

# space between sets not columns
gap <- 20
# width and spacing of columns
width <- 3
space <- 0.4
# empty plot
plot(x = c(0,((width+space)*(length(n)-1))), type = "n",
     y = c(-(max_x+12),(max_y+12)), xlab="", ylab="",
     frame.plot = FALSE, axes = FALSE) 
ref <- 0
maxim <- 0
for(i in 1:length(woon_x)) {
  rect(ref, gap, ref+width, woon_y[i]+gap, col = insect_col)
  ref <- ref + (width+space)
  if((woon_y[i]+gap) > maxim) {
    maxim <- woon_y[i]+gap
  }
}

ref <- 0
for(i in 1:length(woon_x)) {
  rect(ref, -gap, ref+width, -(woon_x[i]+gap), col = rain_col)
  ref <- ref + (width+space)
  
}
axis(side = 2, at = (seq(0, max(rowSums(y)),50)+gap), 
     seq(0, max(rowSums(y)), 50), line = -1.4, cex=2.2)
axis(side = 2, at = -(seq(0,max(rowSums(x)),50)+gap), 
     seq(0, max(rowSums(x)), 50), line = -1.4, cex=2.2)
date.ref <- a[which(a > min(n))]
date.ref <- date.ref[1:3]

for(i in 1:length(date.ref)) {
  text(x = ((date.ref[i]-min(n))*(width+space)-1), 
       y = (max(rowSums(y)) - 10), cex= 1,  
       paste(date.list[date.ref[i]]), pos = 4)
}
abline(v=((a-min(n))*(width+space)))
#par(font=2, mar=c(2, 3, 3, 0), mfcol=c(2,1),
#            cex = 0.6, cex.axis = 1, cex.main = 1)
mtext(side = 3, paste(site, " - Rain clusters (", 
                      toString(rain_clusters),
                      ") and Insect clusters (", 
                      toString(insect_clusters),")"),
      outer = F, cex = 1, line=2)
mtext(side=3,"Days from 22 June to 30 September 2015",
      line=0.6, cex=0.8)

#par(font=1)
mtext(side = 2, "Minutes per day", line = 1.2)
mtext(side = 3, "a.", cex = 1.2, adj = 0.005, outer = TRUE,
      line = -1.8)
# Perform cross correlation on both the Woondum data
#png(paste("plots\\cross-corr_woondum_insects",toString(insect_clusters),"_rain",
#          toString(rain_clusters), "_n_", min(n), "_", max(n), ".png", sep = ""), 
#    height = 450, width = 600)
#par(mar=c(3.8, 3.8, 0, 1), oma=c(0,0,2,0), cex = 1.2, 
#    cex.axis = 1.2)
# empty plot to fill the plot 2 space
plot(c(0, 1440), c(1440, 0), 
     type = "n",axes=FALSE, frame.plot=FALSE,
     xlab="", ylab="")

ylim <- c(-0.2, 0.58)
par(mar=c(3.8, 4.8, 0, 1), oma= c(0,0,0.5,0),
    cex = 0.6, cex.axis = 1, cex.main = 1)
par(new=T)
ccf(woon_y, woon_x, main = "", bty = "n",
    xlab = "", ylab = "", ylim = ylim)
mtext(side=3,"Cross-correlation between rain and insects")
mtext(side = 1, "Lag (days)", line = 2.5)
mtext(side = 2, "Cross-correlation", line = 2.5)
mtext(side = 3, "b.", cex = 1.2, adj = 0.005, outer = TRUE,
      line = -20)
dev.off()
