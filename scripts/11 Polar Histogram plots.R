# Title:  Polar Histogram Plots - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y.F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code generates polar histogram plots. A polar histogram
# is stacked histogram that has been arranged radially around a circle. This
# displays the occurance of the acoustic classes throughout the year.

# File (the cluster list only) and folder requirements
# C:/plos-visualization-paper/data/gympieclusterlist
# C:/plos-visualization-paper/data/woondumclusterlist
# C:/plos-visualization-paper/plots

# Time requirements: about 6 mintues

# Package requirements: plyr, ggplot2

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Polar Histogram 365 days-------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
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


# determine missing minute summary 
missing_minutes_summary <- which(is.na(cluster_list)) 
# determine a list of recorded minutes
z <- setdiff(1:length(cluster_list), missing_minutes_summary)

days <- length(cluster_list)/(1440)
minute_reference <- rep(0:1439, days)

cluster_list <- cbind(cluster_list, minute_reference)
rm(days, minute_reference)

# generate a sequence of dates
start <-  strptime("20150622", format="%Y%m%d")
finish <- strptime("20160723", format="%Y%m%d")
dates <- seq(start, finish, by = "1440 mins")
date.list <- NULL
for (i in 1:length(dates)) {
  dat <- substr(as.character(dates[i]),1,10)
  date.list <- c(date.list, dat)
}

dates <- rep(date.list, each = 1440)
dates <- rep(dates, 2)
# Add site and dates columns to dataframe
sites <- c("Gympie NP", "Woondum NP")
sites <- rep(sites, each=length(dates)/2)
cluster_list <- cbind(cluster_list, sites, dates)
cluster_list <- data.frame(cluster_list)
site <- unique(sites)

print(Sys.time())
# WARNING this section of code takes 2 minutes
# it generates the number of each cluster per day per site
cluster <- NULL
cluster$clust <- 1:(398*2*60)
cluster$count <- 1:(398*2*60)
for(i in 1:length(site)) {
  for(j in 1:length(date.list)) {
    if(i==1) {
      ref <- j*60 - 59  
    }
    if(i==2) {
      ref <- (j*60 - 59) + 398*60
    }
    a <- which(cluster_list$dates==date.list[j] 
               & cluster_list$sites==site[i])
    b <- table(cluster_list[a,1])
    # reorder the rows to a numeric order
    b <- b[c(1, 12, 23, 34, 45, 56, 
             58:60, 2:11, 13:22, 
             24:33, 35:44, 46:55, 57)]
    cluster$site[ref:(ref+59)] <- site[i]
    cluster$date[ref:(ref+59)] <- date.list[j]
    cluster$clust[ref:(ref+59)] <- names(b)
    cluster$count[ref:(ref+59)] <- unname(b) 
    print(j) # of 398 x 2 sites
  }
}
print(Sys.time())
#View(cluster)

cluster <- data.frame(cluster)
length <- length(cluster$clust)/2
gym_clust <- cluster[1:length,]
woon_clust <- cluster[(length+1):(length*2),]
#View(gym_clust)

# rename columns to align to polarHistogram function
gym_clust$score <- gym_clust$clust
gym_clust$value <- gym_clust$count
gym_clust$family <- gym_clust$month
gym_clust$item <- gym_clust$date

woon_clust$score <- woon_clust$clust
woon_clust$value <- woon_clust$count
woon_clust$family <- woon_clust$month
woon_clust$item <- woon_clust$date

# define cluster classes 
rain <- c(2,10,17,18,21,54,59,60) 
wind <- c(9,19,20,24,25,30,40,42,45,46,47,51,52,56)
birds <- c(3,11,14,15,28,33,37,39,43,57,58)
insects <- c(1,4,22,26,27,29)
cicada <- c(7,8,12,16,32,34,44,48)
planes <- c(49,23)
quiet <- c(5,6,13,31,35,36,38,41,50,53,55)
na <- 61

# reduce the 60 clusters into 7 acoustic classes
gympie_clusters <- matrix(ncol = 4, nrow = 398*7, "NA") 
gympie_clusters <- data.frame(gympie_clusters)
colnames(gympie_clusters) <- c("score","value","family","item")

class <- c("rain","birds","insects","cicada","planes","quiet","wind")
class <- rep(class, 398)
gympie_clusters$score <- class
dates1 <- rep(date.list, each=7)
gympie_clusters$dates <- dates1

# fill 'item' column with a single number
gympie_clusters$item <- substr(gympie_clusters$dates,9,10)

# format columns
gympie_clusters$family <- as.character(gympie_clusters$family)
gympie_clusters$item <- as.character(gympie_clusters$item)
gympie_clusters$value <- as.numeric(gympie_clusters$value)

# fill 'family' column with month description
# NOTE: the letter in front of the month is required to keep 
# the order in the polarHistogram function
for(i in 1:length(gympie_clusters$item)) {
  if((substr(gympie_clusters$dates[i],6,7)=="06") &
     (substr(gympie_clusters$dates[i],1,4)=="2015")) {
    gympie_clusters$family[i] <- "a  Jun 2015"
  }  
  if((substr(gympie_clusters$dates[i],6,7)=="06") &
     (substr(gympie_clusters$dates[i],1,4)=="2016")) {
    gympie_clusters$family[i] <- "m  Jun 2016"
  } 
  if((substr(gympie_clusters$dates[i],6,7)=="07") &
     (substr(gympie_clusters$dates[i],1,4)=="2015")) {
    gympie_clusters$family[i] <- "b  Jul 2015"
  }  
  if((substr(gympie_clusters$dates[i],6,7)=="07") &
     (substr(gympie_clusters$dates[i],1,4)=="2016")) {
    gympie_clusters$family[i] <- "n  Jul 2016"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="08") {
    gympie_clusters$family[i] <- "c  Aug 2015"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="09") {
    gympie_clusters$family[i] <- "d  Sept 2015"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="10") {
    gympie_clusters$family[i] <- "e  Oct 2015"
  }  
  if(substr(gympie_clusters$dates[i],6,7)=="11") {
    gympie_clusters$family[i] <- "f  Nov 2015"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="12") {
    gympie_clusters$family[i] <- "g  Dec 2015"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="01") {
    gympie_clusters$family[i] <- "h  Jan 2016"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="02") {
    gympie_clusters$family[i] <- "i  Feb 2016"
  }  
  if(substr(gympie_clusters$dates[i],6,7)=="03") {
    gympie_clusters$family[i] <- "j  Mar 2016"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="04") {
    gympie_clusters$family[i] <- "k  Apr 2016"
  }
  if(substr(gympie_clusters$dates[i],6,7)=="05") {
    gympie_clusters$family[i] <- "l  May 2016"
  } 
}

# find the sum of each acoustic class per day
ref <- 1
for(i in 1:length(date.list)) {
  b <- (which(gym_clust$date==date.list[i]))
  num <- NULL
  for(k in 1:length(rain)) {
    a <- (which(gym_clust$clust==rain[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    rain_num <- unique(num)
  }
  rain_sum <- sum(gym_clust$count[rain_num])
  if(is.na(rain_sum)==TRUE) {
    rain_sum <- 0
  }
  
  num <- NULL
  for(k in 1:length(insects)) {
    a <- (which(gym_clust$clust==insects[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    insects_num <- unique(num)
  }
  insects_sum <- sum(gym_clust$count[insects_num])
  if(is.na(insects_sum)==TRUE) {
    insects_sum <- 0
  }
  
  num <- NULL
  for(k in 1:length(birds)) {
    a <- (which(gym_clust$clust==birds[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    birds_num <- unique(num)
  }
  birds_sum <- sum(gym_clust$count[birds_num])
  if(is.na(birds_sum)==TRUE) {
    birds_sum <- 0
  }
  
  num <- NULL
  for(k in 1:length(quiet)) {
    a <- (which(gym_clust$clust==quiet[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    quiet_num <- unique(num)
  }
  quiet_sum <- sum(gym_clust$count[quiet_num])
  if(is.na(quiet_sum)==TRUE) {
    quiet_sum <- 0
  }
  
  num <- NULL
  for(k in 1:length(cicada)) {
    a <- (which(gym_clust$clust==cicada[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    cicada_num <- unique(num)
  }
  cicada_sum <- sum(gym_clust$count[cicada_num])
  if(is.na(cicada_sum)==TRUE) {
    cicada_sum <- 0
  }
  
  num <- NULL
  for(k in 1:length(planes)) {
    a <- (which(gym_clust$clust==planes[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    planes_num <- unique(num)
  }
  planes_sum <- sum(gym_clust$count[planes_num])
  if(is.na(planes_sum)==TRUE) {
    planes_sum <- 0
  }
  num <- NULL
  for(k in 1:length(wind)) {
    a <- (which(gym_clust$clust==wind[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    wind_num <- unique(num)
  }
  wind_sum <- sum(gym_clust$count[wind_num])
  if(is.na(wind_sum)==TRUE) {
    wind_sum <- 0
  }
  
  gympie_clusters$value[ref:(ref+6)] <- c(rain_sum,
                                          birds_sum,
                                          insects_sum,
                                          cicada_sum,
                                          planes_sum,
                                          quiet_sum,
                                          wind_sum)
  ref <- ref + 7
  print(i) # of 398
}

#View(gympie_clusters)

# Repeat for Woondum site
woondum_clusters <- matrix(ncol = 4, nrow = 398*7, "NA") 
woondum_clusters <- data.frame(woondum_clusters)
colnames(woondum_clusters) <- c("score","value","family","item")

class <- c("rain","birds","insects","cicada","planes","quiet","wind")
class <- rep(class, 398)
woondum_clusters$score <- class
dates1 <- rep(date.list, each=7)
woondum_clusters$dates <- dates1
woondum_clusters$item <- substr(woondum_clusters$dates,9,10)
woondum_clusters$family <- as.character(woondum_clusters$family)
woondum_clusters$item <- as.character(woondum_clusters$item)
woondum_clusters$value <- as.numeric(woondum_clusters$value)
for(i in 1:length(woondum_clusters$item)) {
  if((substr(woondum_clusters$dates[i],6,7)=="06") &
     (substr(woondum_clusters$dates[i],1,4)=="2015")) {
    woondum_clusters$family[i] <- "a  Jun 2015"
  }  
  if((substr(woondum_clusters$dates[i],6,7)=="06") &
     (substr(woondum_clusters$dates[i],1,4)=="2016")) {
    woondum_clusters$family[i] <- "m  Jun 2016"
  } 
  if((substr(woondum_clusters$dates[i],6,7)=="07") &
     (substr(woondum_clusters$dates[i],1,4)=="2015")) {
    woondum_clusters$family[i] <- "b  Jul 2015"
  }  
  if((substr(woondum_clusters$dates[i],6,7)=="07") &
     (substr(woondum_clusters$dates[i],1,4)=="2016")) {
    woondum_clusters$family[i] <- "n  Jul 2016"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="08") {
    woondum_clusters$family[i] <- "c  Aug 2015"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="09") {
    woondum_clusters$family[i] <- "d  Sept 2015"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="10") {
    woondum_clusters$family[i] <- "e  Oct 2015"
  }  
  if(substr(woondum_clusters$dates[i],6,7)=="11") {
    woondum_clusters$family[i] <- "f  Nov 2015"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="12") {
    woondum_clusters$family[i] <- "g  Dec 2015"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="01") {
    woondum_clusters$family[i] <- "h  Jan 2016"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="02") {
    woondum_clusters$family[i] <- "i  Feb 2016"
  }  
  if(substr(woondum_clusters$dates[i],6,7)=="03") {
    woondum_clusters$family[i] <- "j  Mar 2016"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="04") {
    woondum_clusters$family[i] <- "k  Apr 2016"
  }
  if(substr(woondum_clusters$dates[i],6,7)=="05") {
    woondum_clusters$family[i] <- "l  May 2016"
  } 
  print(i) # of 2786
}

ref <- 1
for(i in 1:length(date.list)) {
  b <- (which(woon_clust$date==date.list[i]))
  num <- NULL
  for(k in 1:length(rain)) {
    a <- (which(woon_clust$clust==rain[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    rain_num <- unique(num)
  }
  rain_sum <- sum(woon_clust$count[rain_num])
  if(is.na(rain_sum)==TRUE) {
    rain_sum <- 0
  }
  
  num <- NULL
  for(k in 1:length(insects)) {
    a <- (which(woon_clust$clust==insects[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    insects_num <- unique(num)
  }
  insects_sum <- sum(woon_clust$count[insects_num])
  if(is.na(insects_sum)==TRUE) {
    insects_sum <- 0
  }
  num <- NULL
  for(k in 1:length(birds)) {
    a <- (which(woon_clust$clust==birds[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    birds_num <- unique(num)
  }
  birds_sum <- sum(woon_clust$count[birds_num])
  if(is.na(birds_sum)==TRUE) {
    birds_sum <- 0
  }
  num <- NULL
  for(k in 1:length(quiet)) {
    a <- (which(woon_clust$clust==quiet[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    quiet_num <- unique(num)
  }
  quiet_sum <- sum(woon_clust$count[quiet_num])
  if(is.na(quiet_sum)==TRUE) {
    quiet_sum <- 0
  }
  num <- NULL
  for(k in 1:length(cicada)) {
    a <- (which(woon_clust$clust==cicada[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    cicada_num <- unique(num)
  }
  cicada_sum <- sum(woon_clust$count[cicada_num])
  if(is.na(cicada_sum)==TRUE) {
    cicada_sum <- 0
  }
  num <- NULL
  for(k in 1:length(planes)) {
    a <- (which(woon_clust$clust==planes[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    planes_num <- unique(num)
  }
  planes_sum <- sum(woon_clust$count[planes_num])
  if(is.na(planes_sum)==TRUE) {
    planes_sum <- 0
  }
  num <- NULL
  for(k in 1:length(wind)) {
    a <- (which(woon_clust$clust==wind[k]))
    a <- intersect(a,b)
    num <- c(num, a)
    wind_num <- unique(num)
  }
  wind_sum <- sum(woon_clust$count[wind_num])
  if(is.na(wind_sum)==TRUE) {
    wind_sum <- 0
  }
  woondum_clusters$value[ref:(ref+6)] <- c(rain_sum,
                                           birds_sum,
                                           insects_sum,
                                           cicada_sum,
                                           planes_sum,
                                           quiet_sum,
                                           wind_sum)
  ref <- ref + 7
  print(i) # of 398
}

packages <- c("plyr", "ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# The  polarHistogram365 function code below is an adaption of 
# the polarHistogram function written by Christophe Ladroue 
# Ladroue, C. (2012). Polar histogram  pretty and useful _ 
# Christophe Ladroue. Retrieved from 
# chrisladroue.com/wp-content/uploads/2012/02/polarHistogram.R.zip

# The plyr package is required to run the polarHistogram code
# Wickham, H. (2011). The Split-Apply-Combine Strategy for Data 
# Analysis. Journal of Statistical Software, 40(1), 1-29.
# Retrieved from http://www.jstatsoft.org/v40/i01/
library(plyr)
# The ggplot2 package is required to run the polarHistogram code
# Wickham, H. (2009). ggplot2: Elegant Graphics for Data Analysis.
# New York: Springer-Verlag.
library(ggplot2)

# NOTE the changes to the original code by Ladroue, C. (2012) is 
# indicated by (----) for lines removed and (++++) for lines 
# added and (CCCC) for lines changed
polarHistogram365 <-function (df, family = NULL, 
                              columnNames = NULL, 
                              binSize = 1,
                              spaceItem = 0.2, 
                              spaceFamily = 0, 
                              innerRadius = 0.2, 
                              outerRadius = 1,
                              guides = c(20, 40, 60, 80), 
                              alphaStart = 0, #-0.3,    #(CCCC) 
                              circleProportion = 0.98,
                              direction = "outwards", 
                              familyLabels = TRUE, 
                              normalised = FALSE,
                              units = "cm")
{
  if (!is.null(columnNames)) {
    namesColumn <- names(columnNames)
    names(namesColumn) <- columnNames
    df <- rename(df, namesColumn)
  }
  
  applyLookup <- function(groups, keys, unassigned = "unassigned") {
    lookup <- rep(names(groups), sapply(groups, length, USE.NAMES = FALSE))
    names(lookup) <- unlist(groups, use.names = FALSE)
    p <- lookup[as.character(keys)]
    p[is.na(p)] <- unassigned
    p
  }
  
  if (!is.null(family))
    df$family <- applyLookup(family, df$item)
  df <- arrange(df, family, item, score)
  #if(normalised) #(----)
  df <- ddply(df, .(family, item), transform, 
              value = cumsum(value/(sum(value))))

  df <- ddply(df, .(family, item), transform, previous = c(0, head(value, length(value) - 1)))
  
  df2 <- ddply(df, .(family, item), summarise, indexItem = 1)
  df2$indexItem <- cumsum(df2$indexItem)
  df3 <- ddply(df, .(family), summarise, indexFamily = 1)
  df3$indexFamily <- cumsum(df3$indexFamily)
  df <- merge(df, df2, by = c("family", "item"))
  df <- merge(df, df3, by = "family")
  df <- arrange(df, family, item, score)
  
  affine <- switch(direction,
                   inwards = function(y) (outerRadius - innerRadius) * y + innerRadius,
                   outwards = function(y) (outerRadius - innerRadius) * (1 - y) + innerRadius,
                   stop(paste("Unknown direction")))
  df <- within(df, {
    xmin <- (indexItem - 1) * binSize + (indexItem - 1) *
      spaceItem + (indexFamily - 1) * (spaceFamily - spaceItem)
    xmax <- xmin + binSize
    ymin <- affine(1 - previous)
    ymax <- affine(1 - value)
  })
  
  #if(normalised) #(----)
  guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                         y = rep(1 - guides/100, 1, each = nrow(df)))
  #else           #(----)
  #  guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)), #(----)
  #                         y = rep(1 - guides/maxFamily, 1, each = nrow(df))) #(----)
  
  guidesDF <- within(guidesDF, {
    xend <- xmin + binSize
    y <- affine(y)
  })
  
  totalLength <- tail(df$xmin + binSize + spaceFamily, 1)/circleProportion - 0
  
  p <- ggplot(df) + geom_rect(aes(xmin = xmin, xmax = xmax,
                                  ymin = ymin, ymax = ymax, fill = score))
  readableAngle <- function(x) {
    angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
    angle + ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 180, 0)
  }
  
  readableJustification <- function(x) {
    angle <- x * (-360/totalLength) - alphaStart * 180/pi + 90
    ifelse(sign(cos(angle * pi/180)) + sign(sin(angle * pi/180)) == -2, 1, 0)
  }
  
  dfItemLabels <- ddply(df, .(family, item), summarize, xmin = xmin[1])
  dfItemLabels <- within(dfItemLabels, {
    x <- xmin + binSize/2
    angle <- readableAngle(xmin + binSize/2)
    hjust <- readableJustification(xmin + binSize/2)
  })
  col <- NULL
  for(i in 1:length(dfItemLabels$item)) {
    ifelse(dfItemLabels$item[i]=="01"|dfItemLabels$item[i]=="10"|dfItemLabels$item[i]=="20",
           colour <- "black", colour <- "white")
    col <- c(col, colour)
  }
  
  p <- p + geom_text(aes(x = x, label = item, angle = angle, hjust = hjust), 
                     y = 1.02, size = 1.9, vjust = 0.5, 
                     data = dfItemLabels, 
                     col = col)
  
  p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y),
                        colour = "white", data = guidesDF)
  
  #if(normalised)    #(----)
  guideLabels <- data.frame(x = 0, y = affine(1 - guides/100),
                            label = paste(""))
  #else #(----)
  #  guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily),  #(----)
  #                            label = paste(guides, " ", sep = ""))  #(----)
  
  p <- p + geom_text(aes(x = x, y = y, label = label), data = guideLabels,
                     angle = -alphaStart * 180/pi, hjust = 1, 
                     size = 1.9)
  if (familyLabels) {
    familyLabelsDF <- aggregate(xmin ~ family, data = df,
                                FUN = function(s) mean(s + binSize))
    familyLabelsDF <- within(familyLabelsDF, {
      x <- xmin
      angle <- xmin * (-360/totalLength) - alphaStart * 180/pi
    })
    p <- p + geom_text(aes(x = x, label = family, angle = angle),
                       data = familyLabelsDF, y = 1.1, size = 4)
  }
  
  p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(),
                 axis.title.y = element_blank(), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), axis.text.x = element_blank(),
                 axis.text.y = element_blank(), axis.ticks = element_blank())
  
  p <- p + xlim(0, tail(df$xmin + binSize + spaceFamily, 1)/circleProportion)
  p <- p + ylim(0, outerRadius + 0.2)
  p <- p + coord_polar(start = alphaStart)
  #p <- p + scale_fill_brewer(palette = "Set1", type = "qual")  #(----)
  p <- p + scale_fill_manual(values = c('birds'="#009E73",    #(++++)
                                        'cicada'="#E69F00",   #(++++)
                                        'insects'= "#F0E442", #(++++)
                                        'planes'="#CC79A7",   #(++++)
                                        'rain'="#0072B2",     #(++++)
                                        'wind'="#56B4E9",     #(++++)
                                        'quiet'="#999999"))   #(++++)
  p <- p + theme(legend.text=element_text(size=1))
  p <- p + theme(legend.position="none")
}

gympie_clusters365 <- gympie_clusters

p1 <- polarHistogram365(gympie_clusters365, 
                        familyLabels = TRUE,
                        circleProportion = 0.98,
                        normalised = FALSE)
p1 <- p1 + ggtitle("Gympie NP") + theme(title = element_text(vjust = -6)) + theme(title = element_text(size=20)) 
p1 <- p1 + theme(plot.margin=unit(c(0,-10,0,0),"mm"))
print(p1)
# note the warning received is in regards to the three days 28-30 October
# which account for the 21 rows (3 days x 7 acoustic classes)
ggsave('C:/plos-visualization-paper/plots/polar_histogram_gympie_unedited.tiff', 
       width = 7.5, height = 7.5, dpi = 300, bg = "transparent")

# Repeat for the Woondum National Park recording site
woondum_clusters365 <- woondum_clusters[1:2555,]
woondum_clusters365 <- woondum_clusters

p1 <- polarHistogram365(woondum_clusters365, 
                        familyLabels = TRUE,
                        circleProportion = 0.98,
                        normalised = FALSE)
p1 <- p1 + ggtitle("Woondum NP") + theme(title = element_text(vjust = -6)) + theme(title = element_text(size=20)) 
p1 <- p1 + theme(plot.margin=unit(c(0,-10,0,0),"mm"))
print(p1)
ggsave("C:/plos-visualization-paper/plots/polar_histogram_woondum_unedited.tiff", 
       width = 7.5, height = 7.5, dpi = 300, bg = "transparent")


# Note: editing is required on the saved images 

end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime