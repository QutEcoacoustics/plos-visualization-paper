# Title:  Rose plots - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code generates a number of rose plots using an 
# adaption of code written by Christophe Ladroue 
# Ladroue, C. (2012). Polar histogram  pretty and useful _ 
# Christophe Ladroue. Retrieved from 
# chrisladroue.com/wp-content/uploads/2012/02/polarHistogram.R.zip

# File (the cluster list & sunset times) and folder requirements
# C:/plos-visualization-paper/data/cluster_list.RData
# C:/plos-visualization-paper/data/civil_dawn_2015_2016.RData
# C:/plos-visualization-paper/results
# C:/plos-visualization-paper/plots

# Time requirements: about 65 minutes

# Package requirements
# plyr, ggplot2 - required for polarHistogram function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())

# load the cluster list for the k1 25000 and k2 60 cluster run
load(file="C:/plos-visualization-paper/data/cluster_list.RData")

# generate a 30 minute date sequence
startDate = as.POSIXct("2015-06-22 00:00")
endDate = as.POSIXct("2016-07-24 00:00")
dateSeq30min = substr(seq(from=startDate, to = endDate, by = "30 min"), 1, 16)

# generate a sequence that can be used for thirty-minute sequence
day.ref <- seq(1,(398*2*1440+1), 30) 
# generate half-hour counts for each cluster + sum
# the sum checks for NAs

# WARNING this code takes about 20 minutes
paste(Sys.time(), "Starting counts")
counts <- NULL
for(i in 1:(length(day.ref)-1)) {
  a <- tabulate(cluster_list[day.ref[i]:(day.ref[i+1]-1)], nbins = 60)
  a <- c(a, sum(a))
  counts <- rbind(counts, a)
  rownames(counts) <- 1:nrow(counts)
  print(i) # of 38209
}
write.csv(counts,"C:/plos-visualization-paper/results/counts_30minute.csv", row.names = FALSE)
paste(Sys.time(), "Ending counts")

site <- c("GympieNP", "WoondumNP")
sites <- rep(site, each = (nrow(counts)/2))
dateSeq30min = substr(seq(from=startDate, to = endDate, by = "30 min"), 1, 16)
dateSeq30min <- dateSeq30min[1:(length(dateSeq30min)-1)]
counts <- cbind(counts, sites, dateSeq30min)

# save the 30minute vector
write.csv(counts, "C:/plos-visualization-paper/results/30minute.csv", row.names = FALSE)

min30_data <- read.csv("C:/plos-visualization-paper/results/30minute.csv", header = T)

# generate a time sequence
startDate = as.POSIXct("2013-12-23 00:15:00")
endDate = as.POSIXct("2013-12-23 23:45:00")
dateSeq5sec = seq(from=startDate, to=endDate, by="1800 sec")
head(dateSeq5sec)
times <- substr(dateSeq5sec, 12, 16)

# duplicate times to the length of the data.frame
times <- rep(times, (nrow(clusters)/length(times)) )

# fill the 'family' column with a month description
for(i in 1:nrow(clusters)) {
  if((substr(clusters$dateSeq30min[i],6,7)=="06") &
     (substr(clusters$dateSeq30min[i],1,4)=="2015")) {
    clusters$family[i] <- "a  Jun 15"
  }  
  if((substr(clusters$dateSeq30min[i],6,7)=="06") &
     (substr(clusters$dateSeq30min[i],1,4)=="2016")) {
    clusters$family[i] <- "m  Jun 16"
  } 
  if((substr(clusters$dateSeq30min[i],6,7)=="07") &
     (substr(clusters$dateSeq30min[i],1,4)=="2015")) {
    clusters$family[i] <- "b  Jul 15"
  }  
  if((substr(clusters$dateSeq30min[i],6,7)=="07") &
     (substr(clusters$dateSeq30min[i],1,4)=="2016")) {
    clusters$family[i] <- "n  Jul 16"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="08") {
    clusters$family[i] <- "c  Aug 15"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="09") {
    clusters$family[i] <- "d  Sept 15"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="10") {
    clusters$family[i] <- "e  Oct 15"
  }  
  if(substr(clusters$dateSeq30min[i],6,7)=="11") {
    clusters$family[i] <- "f  Nov 15"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="12") {
    clusters$family[i] <- "g  Dec 15"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="01") {
    clusters$family[i] <- "h  Jan 16"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="02") {
    clusters$family[i] <- "i  Feb 16"
  }  
  if(substr(clusters$dateSeq30min[i],6,7)=="03") {
    clusters$family[i] <- "j  Mar 16"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="04") {
    clusters$family[i] <- "k  Apr 16"
  }
  if(substr(clusters$dateSeq30min[i],6,7)=="05") {
    clusters$family[i] <- "l  May 16"
  }
  print(i) # of 38208
}

# allocate a item column 
clusters$item <- NULL
clusters$item <- substr(clusters$dateSeq30min, 12,16)

# generate a list for the 60 clusters
cluster_names <- NULL
for(i in 1:60) {
  cluster_names <- c(cluster_names, paste("cluster",i,sep = ""))
}


# WARNING lines 135 to 185 takes takes about 40 minutes to run
# this code converts the data to a long format needed for the
# polarHistogram function
data_df <- NULL
for(i in 1:10000) { #nrow(clusters)) {
  a <- matrix(ncol = 4, nrow = 60, "NA") 
  a <- data.frame(a)
  colnames(a) <- c("family", "item", "score", "value")
  a$family <- clusters$family[i] 
  a$score <- cluster_names
  a$item <- clusters$item[i]
  a$value <- t(unname(clusters[i,1:60]))
  data_df <- rbind(data_df, a)
  print(i) # of 38208
}
data_df1 <- NULL
for(i in 10001:20000) { #nrow(clusters)) {
  a <- matrix(ncol = 4, nrow = 60, "NA") 
  a <- data.frame(a)
  colnames(a) <- c("family", "item", "score", "value")
  a$family <- clusters$family[i] 
  a$score <- cluster_names
  a$item <- clusters$item[i] 
  a$value <- t(unname(clusters[i,1:60]))
  data_df1 <- rbind(data_df1, a)
  print(i) # of 38208
}

data_df2 <- NULL
for(i in 20001:30000) { #nrow(clusters)) {
  a <- matrix(ncol = 4, nrow = 60, "NA") 
  a <- data.frame(a)
  colnames(a) <- c("family", "item", "score", "value")
  a$family <- clusters$family[i] 
  a$score <- cluster_names
  a$item <- clusters$item[i] 
  a$value <- t(unname(clusters[i,1:60]))
  data_df2 <- rbind(data_df2, a)
  print(i) # of 38208
}

data_df3 <- NULL
for(i in 30001:nrow(clusters)) {
  a <- matrix(ncol = 4, nrow = 60, "NA") 
  a <- data.frame(a)
  colnames(a) <- c("family", "item", "score", "value")
  a$family <- clusters$family[i] 
  a$score <- cluster_names
  a$item <- clusters$item[i] 
  a$value <- t(unname(clusters[i,1:60]))
  data_df3 <- rbind(data_df3, a)
  print(i) # of 38208
}
df <- rbind(data_df, data_df1, data_df2, data_df3)

# save the dataset in the results folder
write.csv(df, "C:/plos-visualization-paper/results/polar_data.csv", row.names = FALSE)
#rm(data_df, data_df1, data_df2, data_df3) #******************

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Rose plots ---------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())

# if the above code has been run then read the file
f <- paste0("C:/plos-visualization-paper/results/polar_data.csv")
if (file.exists(f)) {
  df <- read.csv(f, TRUE, stringsAsFactors = FALSE)
  rm(f)
}

df$item <- as.character(df$item)
df$family <- as.character(df$family)
df$score <- as.character(df$score)
df$value <- as.numeric(df$value)

# sunrise dataset for the 15th of each month
load("C:/plos-visualization-paper/data/civil_dawn_2015_2016.RData")
a <- which(substr(civil_dawn$dates,9,10)==15)
paste(civil_dawn$dates[a])
civil_dawn <- civil_dawn[a[6:19],1:3]
civil_dawn$Sunrise <- (as.numeric(substr(as.numeric(civil_dawn$Sunrise),1,1))*60) +
  as.numeric(substr(as.numeric(civil_dawn$Sunrise),2,3))
civil_dawn$Sunset <- (as.numeric(substr(as.numeric(civil_dawn$Sunset),1,2))*60) +
  as.numeric(substr(as.numeric(civil_dawn$Sunset),3,4))
sunrise_min <- rep(civil_dawn$Sunrise,2)
sunset_min <- rep(civil_dawn$Sunset,2)

# list the clusters. # NOTE: if other clusters are required
# code stating colours, scale and months for this cluster
# are required see line 175
list1 <- c("cluster37", "cluster44", "cluster48")

# Colours for each class
insect_col <- "#F0E442"
rain_col <- "#0072B2"
wind_col <- "#56B4E9"
bird_col <- "#009E73"
cicada_col <- "#E69F00"
quiet_col <- "#999999"
plane_col <- "#CC79A7"
na_col <- "white"

for(t in 1:length(list1)) {
  clust <- list1[t]
  
  if(clust=="cluster37") {
    col <- bird_col
    scale <- c(20, 20)
    # The selection August 2015 to December 2015 at Gympie
    selection <- 3:7 
    circleProportion <- 0.5
    months <- c("Aug 2015", "Sept 2015", "Oct 2015",
                "Nov 2015", "Dec 2015")
  }
  if(clust=="cluster44"|clust=="cluster48") {
    col <- cicada_col
    circleProportion <- 1
    months <- c("Dec 2015", "Jan 2016", "Feb 2016")
    if(clust=="cluster44") {
      scale <- c(18, 18)
      # The selection - December 2015 to February 2015 at Woondum
      selection <- 21:23 
    }
    if(clust=="cluster48") {
      scale <- c(12, 12)
      selection <- 21:23
    }
  }
  
  # Christophe Ladroue
  library(plyr)
  library(ggplot2)
  
  polarHistogram <-function (df, family = NULL, 
                             columnNames = NULL, 
                             binSize = 1,
                             spaceItem = 0.2, 
                             spaceFamily = 0, 
                             innerRadius = 0, 
                             outerRadius = 10,
                             guides = c(), 
                             alphaStart = 0, #-0.1, 
                             circleProportion = 0.5,
                             direction = "outwards", 
                             familyLabels = FALSE, 
                             normalised = FALSE,
                             labels=FALSE,
                             units = "cm",
                             colour = NULL)
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
    # not useful for date-time data
    #df <- arrange(df, family, item, score)
    if(normalised)
      df <- ddply(df, .(family, item), transform, 
                  value = cumsum(value/(sum(value))))
    else {
      maxFamily <- max(plyr::ddply(df,.(family,item), summarise, total = sum(value))$total)
      df <- ddply(df, .(family, item), transform, value = cumsum(value))
      df$value <- df$value/maxFamily
    }
    
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
    
    if(normalised)
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/100, 1, each = nrow(df)))
    else
      guidesDF <- data.frame(xmin = rep(df$xmin, length(guides)),
                             y = rep(1 - guides/maxFamily, 1, each = nrow(df)))
    
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
    
    if(labels)
      p <- p + geom_text(aes(x = x, label = item, angle = angle,
                             hjust = hjust), y = 1.02, size = 8, vjust = 0.5, data = dfItemLabels)
    # this code prints the guidelines  
    p <- p + geom_segment(aes(x = xmin, xend = xend, y = y, yend = y),
                          colour = "white", data = guidesDF)
    
    if(normalised)
      guideLabels <- data.frame(x = 0, y = affine(1 - guides/100),
                                label = paste(guides, "% ", sep = ""))
    else
      guideLabels <- data.frame(x = 0, y = affine(1 - guides/maxFamily),
                                label = paste(guides, " ", sep = ""), size = 8)
    
    p <- p + geom_text(aes(x = x, y = y, label = label), data = guideLabels,
                       angle = -alphaStart * 180/pi, hjust = 1, size = 8)
    if (familyLabels) {
      familyLabelsDF <- aggregate(xmin ~ family, data = df,
                                  FUN = function(s) mean(s + binSize))
      familyLabelsDF <- within(familyLabelsDF, {
        x <- xmin
        angle <- xmin * (-360/totalLength) - alphaStart * 180/pi
      })
      p <- p + geom_text(aes(x = x, label = family, angle = angle),
                         data = familyLabelsDF, y = 1.35, size = 8)
    }
    
    p <- p + theme(panel.background = element_blank(), axis.title.x = element_blank(),
                   axis.title.y = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.text.x = element_blank(),
                   axis.text.y = element_blank(), axis.ticks = element_blank())
    
    p <- p + xlim(0, tail(df$xmin + binSize + spaceFamily, 1)/circleProportion)
    p <- p + ylim(0, outerRadius + 0.2)
    p <- p + coord_polar(start = alphaStart)
    #p <- p + scale_fill_brewer(palette = "Set1", type = "qual")
    p <- p + scale_fill_manual(values = c('cluster0'="white",
                                          'cluster1'=colour, 'cluster2'=colour, 'cluster3'=colour, 'cluster4'=colour,
                                          'cluster5'=colour, 'cluster6'=colour, 'cluster7'=colour, 'cluster8'=colour,
                                          'cluster9'=colour, 'cluster10'=colour,'cluster11'=colour, 'cluster12'=colour,
                                          'cluster13'=colour, 'cluster14'=colour,'cluster15'=colour, 'cluster16'=colour,
                                          'cluster17'=colour, 'cluster18'=colour,'cluster19'=colour, 'cluster20'=colour,
                                          'cluster21'=colour, 'cluster22'=colour,'cluster23'=colour, 'cluster24'=colour,
                                          'cluster25'=colour, 'cluster26'=colour, 'cluster27'=colour, 'cluster28'=colour,
                                          'cluster29'=colour, 'cluster30'=colour, 'cluster31'=colour, 'cluster32'=colour,
                                          'cluster33'=colour, 'cluster34'=colour,'cluster35'=colour, 'cluster36'=colour,
                                          'cluster37'=colour, 'cluster38'=colour,'cluster39'=colour, 'cluster40'=colour,
                                          'cluster41'=colour, 'cluster42'=colour,'cluster43'=colour, 'cluster44'=colour,
                                          'cluster45'=colour, 'cluster46'=colour,'cluster47'=colour, 'cluster48'=colour,
                                          'cluster49'=colour, 'cluster50'=colour, 'cluster51'=colour, 'cluster52'=colour,
                                          'cluster53'=colour, 'cluster54'=colour, 'cluster55'=colour, 'cluster56'=colour,
                                          'cluster57'=colour, 'cluster58'=colour,'cluster59'=colour, 'cluster60'=colour))
    p <- p + theme(legend.text=element_text(size=1))
    p <- p + theme(legend.position="none")
    p <<- p
  } 
  # End of polarHistogram function -------------------------
  
  # shift the time labels by 15 minutes to correspond to the 
  # middle of each time period
  times<- unique(df$item)
  
  startDate = as.POSIXct("2013-12-23 00:15:00")
  endDate = as.POSIXct("2013-12-23 23:45:00")
  dateSeq5sec = seq(from=startDate, to=endDate, by="1800 sec")
  
  times_new <- substr(dateSeq5sec, 12, 16)
  
  for(i in 1:length(times)) {
    a <- which(df$item==times[i]) 
    df$item[a] <- times_new[i]
  }
  
  #df$item <- substr(df$item, 1,5)
  b <- nrow(df)
  gym_df <- df[1:(b/2),]
  won_df <- df[(b/2+1):b,]
  
  a <- which(gym_df$score==clust)
  gym_df <- gym_df[a,]
  
  a <- which(won_df$score==clust)
  won_df <- won_df[a,]
  
  #a <- which(gym_df$value==23)
  #m <- max(gym_df$value)
  
  # June 2015
  a <- which(gym_df$family=="a  Jun 15")
  b <- c(min(a), max(a))
  GympieNP_June2015 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_June2015)/48
  # normalise the number of minutes
  GympieNP_June2015$value <- GympieNP_June2015$value/n
  # set the scale of the polar plot by setting an empty
  # time slot to the scale set above
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_June2015$item==times_new[i] & GympieNP_June2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  # if a is not NULL take the last value
  if(length(a) >= 1) {
    GympieNP_June2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_June2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_June2015$value)
    a <- which(GympieNP_June2015$value==min)
    GympieNP_June2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_June2015$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="a  Jun 15")
  b <- c(min(a), max(a))
  WoondumNP_June2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_June2015)/48
  # normalise the number of minutes
  WoondumNP_June2015$value <- WoondumNP_June2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_June2015$item==times_new[i] & WoondumNP_June2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_June2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_June2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_June2015$value)
    a <- which(WoondumNP_June2015$value==min)
    WoondumNP_June2015$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_June2015$score[a[1]] <- "cluster0"
  }
  
  # July 2015
  a <- which(gym_df$family=="b  Jul 15")
  b <- c(min(a), max(a))
  GympieNP_July2015 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_July2015)/48
  # normalise the number of minutes
  GympieNP_July2015$value <- GympieNP_July2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_July2015$item==times_new[i] & GympieNP_July2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_July2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_July2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_July2015$value)
    a <- which(GympieNP_July2015$value==min)
    GympieNP_July2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_July2015$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="b  Jul 15")
  b <- c(min(a), max(a))
  WoondumNP_July2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_July2015)/48
  # normalise the number of minutes
  WoondumNP_July2015$value <- WoondumNP_July2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_July2015$item==times_new[i] & WoondumNP_July2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_July2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_July2015$score[a[length(a)]] <- "cluster0"
  }
  
  if(length(a) < 1) {
    min <- min(WoondumNP_July2015$value)
    a <- which(WoondumNP_July2015$value==min)
    WoondumNP_July2015$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_July2015$score[a[1]] <- "cluster0"
  }
  
  # August 2015
  a <- which(gym_df$family=="c  Aug 15")
  b <- c(min(a), max(a))
  GympieNP_August2015 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_August2015)/48
  # normalise the number of minutes
  GympieNP_August2015$value <- GympieNP_August2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_August2015$item==times_new[i] & GympieNP_August2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_August2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_August2015$score[a[length(a)]] <- "cluster0"
  }
  
  if(length(a) < 1) {
    min <- min(GympieNP_August2015$value)
    a <- which(GympieNP_August2015$value==min)
    GympieNP_August2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_August2015$score[a[1]] <- "cluster0"
  }
  
  
  a <- which(won_df$family=="c  Aug 15")
  b <- c(min(a), max(a))
  WoondumNP_August2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_August2015)/48
  # normalise the number of minutes
  WoondumNP_August2015$value <- WoondumNP_August2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_August2015$item==times_new[i] & WoondumNP_August2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_August2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_August2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_August2015$value)
    a <- which(WoondumNP_August2015$value==min)
    WoondumNP_August2015$value[a[1]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_August2015$score[a[1]] <- "cluster0"
  }
  
  # September 2015
  m_days <- c(28,29,30) # place the missing dates, must be in numeric order
  a <- which(gym_df$family=="d  Sept 15")
  b <- c(min(a), max(a))
  GympieNP_September2015 <- gym_df[b[1]:b[2],]
  if(length(m_days >= 1)) {
    for(i in 1:length(m_days)) {
      GympieNP_September2015 <- GympieNP_September2015[-(((m_days[i]-1)*48+1):(m_days[i]*48)),]    
      m_days <- m_days - 1
    }
  }
  # determine the number of days
  n <- (nrow(GympieNP_September2015)/48) 
  # normalise the number of minutes
  GympieNP_September2015$value <- GympieNP_September2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_September2015$item==times_new[i] & GympieNP_September2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  
  if(length(a) >= 1) {
    GympieNP_September2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_September2015$score[a[length(a)]] <- "cluster0"
  }
  
  if(length(a) < 1) {
    min <- min(GympieNP_September2015$value)
    a <- which(GympieNP_September2015$value==min)
    GympieNP_September2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_September2015$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="d  Sept 15")
  b <- c(min(a), max(a))
  WoondumNP_September2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_September2015)/48
  # normalise the number of minutes
  WoondumNP_September2015$value <- WoondumNP_September2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_September2015$item==times_new[i] & WoondumNP_September2015$value < 0.04)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_September2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_September2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_September2015$value)
    a <- which(WoondumNP_September2015$value==min)
    WoondumNP_September2015$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_September2015$score[a[1]] <- "cluster0"
  }
  
  # October 2015
  a <- which(gym_df$family=="e  Oct 15")
  b <- c(min(a), max(a))
  GympieNP_October2015 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_October2015)/48
  # normalise the number of minutes
  GympieNP_October2015$value <- GympieNP_October2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_October2015$item==times_new[i] & GympieNP_October2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_October2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_October2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_October2015$value)
    a <- which(GympieNP_October2015$value==min)
    GympieNP_October2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_October2015$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="e  Oct 15")
  b <- c(min(a), max(a))
  WoondumNP_October2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_October2015)/48
  # normalise the number of minutes
  WoondumNP_October2015$value <- WoondumNP_October2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_October2015$item==times_new[i] & WoondumNP_October2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_October2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_October2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_October2015$value)
    a <- which(WoondumNP_October2015$value==min)
    WoondumNP_October2015$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_October2015$score[a[1]] <- "cluster0"
  }
  
  # November 2015
  a <- which(gym_df$family=="f  Nov 15")
  b <- c(min(a), max(a))
  GympieNP_November2015 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_November2015)/48
  # normalise the number of minutes
  GympieNP_November2015$value <- GympieNP_November2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_November2015$item==times_new[i] & GympieNP_November2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_November2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_November2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_November2015$value)
    a <- which(GympieNP_November2015$value==min)
    GympieNP_November2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_November2015$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="f  Nov 15")
  b <- c(min(a), max(a))
  WoondumNP_November2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_November2015)/48
  # normalise the number of minutes
  WoondumNP_November2015$value <- WoondumNP_November2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_November2015$item==times_new[i] & WoondumNP_November2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_November2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_November2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_November2015$value)
    a <- which(WoondumNP_November2015$value==min)
    WoondumNP_November2015$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_November2015$score[a[1]] <- "cluster0"
  }
  
  # December 2015
  a <- which(gym_df$family=="g  Dec 15")
  b <- c(min(a), max(a))
  GympieNP_December2015 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_December2015)/48
  # normalise the number of minutes
  GympieNP_December2015$value <- GympieNP_December2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_December2015$item==times_new[i] & GympieNP_December2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_December2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_December2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_December2015$value)
    a <- which(GympieNP_December2015$value==min)
    GympieNP_December2015$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_December2015$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="g  Dec 15")
  b <- c(min(a), max(a))
  WoondumNP_December2015 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_December2015)/48
  # normalise the number of minutes
  WoondumNP_December2015$value <- WoondumNP_December2015$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_December2015$item==times_new[i] & WoondumNP_December2015$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_December2015$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_December2015$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_December2015$value)
    a <- which(WoondumNP_December2015$value==min)
    WoondumNP_December2015$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_December2015$score[a[1]] <- "cluster0"
  }
  
  # January 2016
  a <- which(gym_df$family=="h  Jan 16")
  b <- c(min(a), max(a))
  GympieNP_January2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_January2016)/48
  # normalise the number of minutes
  GympieNP_January2016$value <- GympieNP_January2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_January2016$item==times_new[i] & GympieNP_January2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_January2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_January2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_January2016$value)
    a <- which(GympieNP_January2016$value==min)
    GympieNP_January2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_January2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="h  Jan 16")
  b <- c(min(a), max(a))
  WoondumNP_January2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_January2016)/48
  # normalise the number of minutes
  WoondumNP_January2016$value <- WoondumNP_January2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_January2016$item==times_new[i] & WoondumNP_January2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_January2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_January2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_January2016$value)
    a <- which(WoondumNP_January2016$value==min)
    WoondumNP_January2016$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_January2016$score[a[1]] <- "cluster0"
  }
  
  # February 2016
  a <- which(gym_df$family=="i  Feb 16")
  b <- c(min(a), max(a))
  GympieNP_February2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_February2016)/48
  # normalise the number of minutes
  GympieNP_February2016$value <- GympieNP_February2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_February2016$item==times_new[i] & GympieNP_February2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_February2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_February2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    a <- which(GympieNP_February2016$value < 0.1)
    min <- min(GympieNP_February2016$value)
    a <- which(GympieNP_February2016$value==min)
    GympieNP_February2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_February2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="i  Feb 16")
  b <- c(min(a), max(a))
  WoondumNP_February2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_February2016)/48
  # normalise the number of minutes
  WoondumNP_February2016$value <- WoondumNP_February2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_February2016$item==times_new[i] & WoondumNP_February2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_February2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_February2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_February2016$value)
    a <- which(WoondumNP_February2016$value==min)
    WoondumNP_February2016$value[a[1]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_February2016$score[a[1]] <- "cluster0"
  }
  
  # March 2016
  a <- which(gym_df$family=="j  Mar 16")
  b <- c(min(a), max(a))
  GympieNP_March2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_March2016)/48
  # normalise the number of minutes
  GympieNP_March2016$value <- GympieNP_March2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_March2016$item==times_new[i] & GympieNP_March2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_March2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_March2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_March2016$value)
    a <- which(GympieNP_March2016$value==min)
    GympieNP_March2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_March2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="j  Mar 16")
  b <- c(min(a), max(a))
  WoondumNP_March2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_March2016)/48
  # normalise the number of minutes
  WoondumNP_March2016$value <- WoondumNP_March2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_March2016$item==times_new[i] & WoondumNP_March2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_March2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_March2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_March2016$value)
    a <- which(WoondumNP_March2016$value==min)
    WoondumNP_March2016$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_March2016$score[a[1]] <- "cluster0"
  }
  
  # April 2016
  a <- which(gym_df$family=="k  Apr 16")
  b <- c(min(a), max(a))
  GympieNP_April2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_April2016)/48
  # normalise the number of minutes
  GympieNP_April2016$value <- GympieNP_April2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_April2016$item==times_new[i] & GympieNP_April2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_April2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_April2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_April2016$value)
    a <- which(GympieNP_April2016$value==min)
    GympieNP_April2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_April2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="k  Apr 16")
  b <- c(min(a), max(a))
  WoondumNP_April2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_April2016)/48
  # normalise the number of minutes
  WoondumNP_April2016$value <- WoondumNP_April2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_April2016$item==times_new[i] & WoondumNP_April2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_April2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_April2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_April2016$value)
    a <- which(WoondumNP_April2016$value==min)
    WoondumNP_April2016$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_April2016$score[a[1]] <- "cluster0"
  }
  
  # May 2016
  a <- which(gym_df$family=="l  May 16")
  b <- c(min(a), max(a))
  GympieNP_May2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_May2016)/48
  # normalise the number of minutes
  GympieNP_May2016$value <- GympieNP_May2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_May2016$item==times_new[i] & GympieNP_May2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_May2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_May2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_May2016$value)
    a <- which(GympieNP_May2016$value==min)
    GympieNP_May2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_May2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="l  May 16")
  b <- c(min(a), max(a))
  WoondumNP_May2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_May2016)/48
  # normalise the number of minutes
  WoondumNP_May2016$value <- WoondumNP_May2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_May2016$item==times_new[i] & WoondumNP_May2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_May2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_May2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_May2016$value)
    a <- which(WoondumNP_May2016$value==min)
    WoondumNP_May2016$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_May2016$score[a[1]] <- "cluster0"
  }
  
  # June 2016
  a <- which(gym_df$family=="m  Jun 16")
  b <- c(min(a), max(a))
  GympieNP_June2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_June2016)/48
  # normalise the number of minutes
  GympieNP_June2016$value <- GympieNP_June2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_June2016$item==times_new[i] & GympieNP_June2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_June2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_June2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_June2016$value)
    a <- which(GympieNP_June2016$value==min)
    GympieNP_June2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_June2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="m  Jun 16")
  b <- c(min(a), max(a))
  WoondumNP_June2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_June2016)/48
  # normalise the number of minutes
  WoondumNP_June2016$value <- WoondumNP_June2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_June2016$item==times_new[i] & WoondumNP_June2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_June2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_June2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(WoondumNP_June2016$value)
    a <- which(WoondumNP_June2016$value==min)
    WoondumNP_June2016$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_June2016$score[a[1]] <- "cluster0"
  }
  
  # July 2016
  a <- which(gym_df$family=="n  Jul 16")
  b <- c(min(a), max(a))
  GympieNP_July2016 <- gym_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(GympieNP_July2016)/48
  # normalise the number of minutes
  GympieNP_July2016$value <- GympieNP_July2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(GympieNP_July2016$item==times_new[i] & GympieNP_July2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    GympieNP_July2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    GympieNP_July2016$score[a[length(a)]] <- "cluster0"
  }
  if(length(a) < 1) {
    min <- min(GympieNP_July2016$value)
    a <- which(GympieNP_July2016$value==min)
    GympieNP_July2016$value[a[1]] <- (scale[1] - 0.5)
    GympieNP_July2016$score[a[1]] <- "cluster0"
  }
  
  a <- which(won_df$family=="n  Jul 16")
  b <- c(min(a), max(a))
  WoondumNP_July2016 <- won_df[b[1]:b[2],]
  # determine the number of days
  n <- nrow(WoondumNP_July2016)/48
  # normalise the number of minutes
  WoondumNP_July2016$value <- WoondumNP_July2016$value/n
  # set the scale of the polar plot
  a <- NULL
  for(i in 1:length(times_new)) {
    ref <- which(WoondumNP_July2016$item==times_new[i] & WoondumNP_July2016$value< 0.4)
    if(length(ref)==n) {
      a <- c(a,ref[n])  
    }
  }
  if(length(a) >= 1) {
    WoondumNP_July2016$value[a[length(a)]] <- (scale[1] - 0.5)
    # cluster0 will be white
    WoondumNP_July2016$score[a[length(a)]] <- "cluster0"
  }
  
  if(length(a) < 1) {
    min <- min(WoondumNP_July2016$value)
    a <- which(WoondumNP_July2016$value==min)
    WoondumNP_July2016$value[a[1]] <- (scale[1] - 0.5)
    WoondumNP_July2016$score[a[1]] <- "cluster0"
  }
  a <- c("GympieNP_June2015", "GympieNP_July2015",
         "GympieNP_August2015", "GympieNP_September2015",
         "GympieNP_October2015", "GympieNP_November2015",
         "GympieNP_December2015", "GympieNP_January2016",
         "GympieNP_February2016", "GympieNP_March2016",
         "GympieNP_April2016", "GympieNP_May2016",
         "GympieNP_June2016", "GympieNP_July2016",
         "WoondumNP_June2015", "WoondumNP_July2015",
         "WoondumNP_August2015", "WoondumNP_September2015",
         "WoondumNP_October2015", "WoondumNP_November2015",
         "WoondumNP_December2015", "WoondumNP_January2016",
         "WoondumNP_February2016", "WoondumNP_March2016",
         "WoondumNP_April2016", "WoondumNP_May2016",
         "WoondumNP_June2016", "WoondumNP_July2016")
  
  a37 <- a[selection]
  a37 <- data.frame(a37)
  a37[,2] <- NULL
  a37[,2] <- selection
  # Set the layout matrix to divide page into two frames one
  # for the plot and one for the table
  layout.show(11)
  
  n <- 1:nrow(a37)
  j <- 0
  for(i in a37[n,1]) {
    j <- j + 1
    r <- a37[j,2]
    title <- paste(i)
    subtitle <- paste("Cluster", substr(clust,8,10))
    file_title <- paste("C:/plos-visualization-paper/plots/rose_plot", i,"_",clust, ".tiff",sep = "")
    data <- get(i)
    data <- data.frame(data)
    if(clust=="cluster37") {
      list <- c("00:15","00:45","01:15","01:45","02:15","02:45","03:15","03:45","04:15",
                "04:45","05:15","05:45","06:15","06:45","07:15","07:45","08:15","08:45",
                "09:15","09:45","10:15","10:45","11:15","11:45","12:15","12:30")
      ac <- which(data$item=="00:15")
      data$value[ac[1]] <- (as.numeric(scale[1]) - 0.5)
      data$score[ac[1]] <- "cluster0"
      ab <- NULL
      for(k in 1:length(list)) {
        aa <- which(data$item==list[k])
        ab <- c(ab, aa)
      }
      ab <- sort(ab)
    }
    if(clust=="cluster44"|clust=="cluster48") {
      ab <- 1:nrow(data)
    }
    if(clust=="cluster37") {
      z <- polarHistogram(data[ab,], familyLabels = F, normalised = F, 
                          colour = col, circleProportion = 0.5,
                          innerRadius = 0, outerRadius = 1,
                          guides = seq(2,(scale[1]-1),2), 
                          labels = TRUE)
    }
    if(clust=="cluster44"|clust=="cluster48") {
      z <- polarHistogram(data[ab,], familyLabels = F, normalised = F, 
                          colour = col, circleProportion = 1,
                          innerRadius = 0, outerRadius = 1,
                          guides = seq(2,(scale[1]-1),2), 
                          labels = TRUE)
    }
    #z <- z + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), ""))))
    #z <- z + theme(plot.title = element_text(size=22))
    #z <- z + theme(plot.title = element_text(margin=margin(b = -50, unit = "pt")))
    # add the sun symbol x is angle and y the fraction of the radius
    z <- z + geom_point(data=data.frame(x=c(1)), 
                        aes(x = (sunrise_min[r]*60/1440-0.5), y = 0.93), 
                        shape="\u2600", size=11)
    if(clust=="cluster44"|clust=="cluster48") {
      # add the moon symbol 
      z <- z + geom_point(data=data.frame(x=c(1)), 
                          aes(x = (sunset_min[r]*60/1440-2), y = 0.93), 
                          shape="\u263D", size=11)
    } 
    if(clust=="cluster37"|clust=="cluster44"){
      # add the month label
      z <- z + annotate("text", x=0.4*scale[1], y=0.55, 
                        label= months[j], size = 11)
    }
    if(clust=="cluster48") {
      # add the month label
      z <- z + annotate("text", x=0.6*scale[1], y=0.55, 
                        label= months[j], size = 11)
      
    }
    #z <- z + geom_point(x=150,y=0,shape="\u2600", size=20)
    z <- z + theme(plot.background = element_rect(fill = "transparent", 
                                                  colour = NA))
    if(clust=="cluster37") {
      # plot margin controls the top, right, bottom, and left margins
      z <- z + theme(plot.margin=unit(c(-15,-13, -15,-95),"mm"))  
    }
    if(clust=="cluster44"|clust=="cluster48") {
      # plot margin controls the top, right, bottom, and left margins
      z <- z + theme(plot.margin=unit(c(-14,-19, -15,-21),"mm"))  
    }
    if(clust=="cluster37") {
      if(j==1) {
        O <- z 
      }
      if(j==2) {
        P <- z 
      }
      if(j==3) {
        Q <- z 
      }
      if(j==4) {
        R <- z 
      }
      if(j==5) {
        S <- z 
      }
    }
    if(clust=="cluster44") {
      if(j==1) {
        U <- z 
      }
      if(j==2) {
        V <- z 
      }
      if(j==3) {
        W <- z 
      }
    }
    if(clust=="cluster48") {
      if(j==1) {
        X <- z 
      }
      if(j==2) {
        Y <- z 
      }
      if(j==3) {
        Z <- z 
      }
    }
    #invisible is used to stop print opening a print window
    invisible(print(z))
    if(clust=="cluster37") {
      ggsave(file_title, width = 10.2, height = 19.05, units = "cm", 
             dpi = 300)  
    }
    if(clust=="cluster44"|clust=="cluster48") {
      ggsave(file_title, width = 19.05, height = 19.05, units = "cm", 
             dpi = 300)  
    }
    dev.off()
  }
  print(t) # of 3 clusters
}