# Title:  Twelve day Testing - Plos One paper
# Author: Yvonne Phillips
# Date:  20 September 2016

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code generates hclust dendrograms based on the
# twelve acoustic signatures. The twelved acoustic signatures are 
# calculated from the twelve days described in the paper mentioned above.
# The heights each dendrogram are used to calculate the I3DD values.
# Low I3DD values indicates that the four groups of three days are well
# separated. This code also saves a copy of each twelve day set of the
# acoustic indices for each k1 and k2 combination.
# For each cluster run there are 20 .csv files and 20 dendrograms that 
# are saved and one I3DD file.

# File and folder requirements (1 file and 1 folder): 
# Each hybrid clustering run generates a file containing the cluster
# list for each k2 values (5,10,15,20,...95,100), for example
# C:/plos-visualization-paper/results/hclust_clusters_25000.Rdata

# Time requirements: about 1 mintue

#########################################################
# Get the cluster list corresponding to the twelve days
#########################################################
# remove all objects in the global environment
rm(list = ls())

folder <- "C:/plos-visualization-paper/results"
myFiles <- list.files(path=folder, recursive=T, full.names=TRUE, 
                      pattern="hclust*.*")
myFiles

# Determine the single starting minute for each of the twelve days
day1 <-  38*1440+1  # day 39 (30 July 2015 Gympie)
day2 <-  39*1440+1  # day 40 (31 July 2015 Gympie)
day3 <-  40*1440+1  # day 41 (1 August 2015 Gympie)
day4 <-  70*1440+1  # day 71 (31 August 2015 Gympie)
day5 <-  71*1440+1  # day 72 (1 September 2015 Gympie)
day6 <-  74*1440+1  # day 75 (4 September 2015 Gympie)
day7 <-  (38+398)*1440+1  # day 39 + 398 (30 July 2015 Woondum)
day8 <-  (39+398)*1440+1  # day 40 + 398 (31 July 2015 Woondum)
day9 <-  (40+398)*1440+1  # day 41 + 398 (1 August 2015 Woondum)
day10 <- (70+398)*1440+1  # day 71 + 398 (31 August 2015 Woondum)
day11 <- (71+398)*1440+1  # day 72 + 398 (1 September 2015 Woondum)
day12 <- (74+398)*1440+1  # day 75 + 398 (4 September 2015 Woondum)

days <- c(day1, day2, day3, day4, day5, day6,
          day7, day8, day9, day10, day11, day12)

load(myFiles[1])

actual_start_date <- "2015-06-22" # the first day of the recording 
actual_end_date <- "2016-07-23"
interval <- 1440
start <- as.POSIXct(actual_start_date)
end <- as.POSIXct(actual_end_date)
dates <- seq(from=start, 
             by=interval*60, to=end)
dates <- rep(rep(dates, each=1440),2)

# Generate a series of dataframes (clusters25000, etc.) 
# containing the cluster list for the twelve days
clusters25000 <- NULL
for (i in 1:ncol(hclust_clusters_25000)) {
  cluster_list <- hclust_clusters_25000[c((days[1]:(days[1]+1439)),
                                          (days[2]:(days[2]+1439)),
                                          (days[3]:(days[3]+1439)),
                                          (days[4]:(days[4]+1439)),
                                          (days[5]:(days[5]+1439)),
                                          (days[6]:(days[6]+1439)),
                                          (days[7]:(days[7]+1439)),
                                          (days[8]:(days[8]+1439)),
                                          (days[9]:(days[9]+1439)),
                                          (days[10]:(days[10]+1439)),
                                          (days[11]:(days[11]+1439)),
                                          (days[12]:(days[12]+1439))),i]
  
  clusters25000 <- cbind(clusters25000, cluster_list)
}

colnames(clusters25000) <- c("clusters5", "clusters10", "clusters15", 
                             "clusters20", "clusters25", "clusters30", 
                             "clusters35", "clusters40", "clusters45", 
                             "clusters50", "clusters55", "clusters60", 
                             "clusters65", "clusters70", "clusters75", 
                             "clusters80", "clusters85", "clusters90", 
                             "clusters95", "clusters100")

save(clusters25000, file="C:/plos-visualization-paper/results/Clusters25000_summary.RData")

######################################################
# Saving the Hybrid 24 hour files containing the 
# twenty-four hour acoustic signatures
####################################################
day.ref <- c(seq(1,(12*1440),1440), ((12*1440)+1))

# "Twenty-four-hour" function
# This function calculates the acoustic signatures and saves a .csv
# file for each k1 and k2 value combination

twenty_four_hour <- function(cluster.list, csv.name) {
  folder <- "C:/plos-visualization-paper/results/"
  myFilesShort <- list.files(full.names=FALSE, 
                             path=folder, pattern="hclust_clusters")
  k1values <- substr(myFilesShort,17,21)
  for (l in 1:length(myFilesShort)) {
    load(paste(folder, "hclust_clusters_", k1values[l], ".RData", sep=""))
  }
  twentyfour_hour_table_1  <- matrix(0, nrow = 12, ncol = 5)
  twentyfour_hour_table_2  <- matrix(0, nrow = 12, ncol = 10)
  twentyfour_hour_table_3  <- matrix(0, nrow = 12, ncol = 15)
  twentyfour_hour_table_4  <- matrix(0, nrow = 12, ncol = 20)
  twentyfour_hour_table_5  <- matrix(0, nrow = 12, ncol = 25)
  twentyfour_hour_table_6  <- matrix(0, nrow = 12, ncol = 30)
  twentyfour_hour_table_7  <- matrix(0, nrow = 12, ncol = 35)
  twentyfour_hour_table_8  <- matrix(0, nrow = 12, ncol = 40)
  twentyfour_hour_table_9  <- matrix(0, nrow = 12, ncol = 45)
  twentyfour_hour_table_10 <- matrix(0, nrow = 12, ncol = 50)
  twentyfour_hour_table_11 <- matrix(0, nrow = 12, ncol = 55)
  twentyfour_hour_table_12 <- matrix(0, nrow = 12, ncol = 60)
  twentyfour_hour_table_13 <- matrix(0, nrow = 12, ncol = 65)
  twentyfour_hour_table_14 <- matrix(0, nrow = 12, ncol = 70)
  twentyfour_hour_table_15 <- matrix(0, nrow = 12, ncol = 75)
  twentyfour_hour_table_16 <- matrix(0, nrow = 12, ncol = 80)
  twentyfour_hour_table_17 <- matrix(0, nrow = 12, ncol = 85)
  twentyfour_hour_table_18 <- matrix(0, nrow = 12, ncol = 90)
  twentyfour_hour_table_19 <- matrix(0, nrow = 12, ncol = 95)
  twentyfour_hour_table_20 <- matrix(0, nrow = 12, ncol = 100)
  #cluster_length <- seq(5, 100, 5)
  for (i in 1:ncol(cluster.list)) {
    for(j in 1:(length(day.ref)-1)) {
      #cluster.ref <- hist(cluster.list[day.ref[j]:(day.ref[j+1]-1),i], 
      #                    breaks=seq(0.5, cluster_length[i] + 0.5))
      
      cluster.ref <- table(cluster.list[(day.ref[j]:(day.ref[j+1]-1)), i])
      names <- unique(names(cluster.ref))
      
      for(m in 1:length(names)) {  
        name <- names[m]
        if(i == 1)  {twentyfour_hour_table_1[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 2)  {twentyfour_hour_table_2[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 3)  {twentyfour_hour_table_3[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 4)  {twentyfour_hour_table_4[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 5)  {twentyfour_hour_table_5[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 6)  {twentyfour_hour_table_6[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 7)  {twentyfour_hour_table_7[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 8)  {twentyfour_hour_table_8[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 9)  {twentyfour_hour_table_9[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 10) {twentyfour_hour_table_10[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 11) {twentyfour_hour_table_11[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 12) {twentyfour_hour_table_12[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 13) {twentyfour_hour_table_13[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 14) {twentyfour_hour_table_14[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 15) {twentyfour_hour_table_15[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 16) {twentyfour_hour_table_16[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 17) {twentyfour_hour_table_17[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 18) {twentyfour_hour_table_18[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 19) {twentyfour_hour_table_19[j,as.numeric(name)] <- cluster.ref[[m]]}
        if(i == 20) {twentyfour_hour_table_20[j,as.numeric(name)] <- cluster.ref[[m]]}
      }
    }
  }
  # Rename the columns
  column.names <- NULL
  for (k in 1:(ncol(twentyfour_hour_table_20))) {
    col.names <- paste("clus_", k, sep = "")
    column.names <- c(column.names, col.names)
  }
  colnames(twentyfour_hour_table_20) <- column.names
  
  column.names <- column.names[1:95]
  colnames(twentyfour_hour_table_19) <- column.names
  
  column.names <- column.names[1:90]
  colnames(twentyfour_hour_table_18) <- column.names
  
  column.names <- column.names[1:85]
  colnames(twentyfour_hour_table_17) <- column.names
  
  column.names <- column.names[1:80]
  colnames(twentyfour_hour_table_16) <- column.names
  
  column.names <- column.names[1:75]
  colnames(twentyfour_hour_table_15) <- column.names
  
  column.names <- column.names[1:70]
  colnames(twentyfour_hour_table_14) <- column.names
  
  column.names <- column.names[1:65]
  colnames(twentyfour_hour_table_13) <- column.names
  
  column.names <- column.names[1:60]
  colnames(twentyfour_hour_table_12) <- column.names
  
  column.names <- column.names[1:55]
  colnames(twentyfour_hour_table_11) <- column.names
  
  column.names <- column.names[1:50]
  colnames(twentyfour_hour_table_10) <- column.names
  
  column.names <- column.names[1:45]
  colnames(twentyfour_hour_table_9) <- column.names
  
  column.names <- column.names[1:40]
  colnames(twentyfour_hour_table_8) <- column.names
  
  column.names <- column.names[1:35]
  colnames(twentyfour_hour_table_7) <- column.names
  
  column.names <- column.names[1:30]
  colnames(twentyfour_hour_table_6) <- column.names
  
  column.names <- column.names[1:25]
  colnames(twentyfour_hour_table_5) <- column.names
  
  column.names <- column.names[1:20]
  colnames(twentyfour_hour_table_4) <- column.names
  
  column.names <- column.names[1:15]
  colnames(twentyfour_hour_table_3) <- column.names
  
  column.names <- column.names[1:10]
  colnames(twentyfour_hour_table_2) <- column.names
  
  column.names <- column.names[1:5]
  colnames(twentyfour_hour_table_1) <- column.names
  
  write.csv(twentyfour_hour_table_1, paste(folder, csv.name, "_k005_24hour.csv", sep = ""), 
            row.names = F)
  write.csv(twentyfour_hour_table_2, paste(folder, csv.name, "_k010_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_3, paste(folder, csv.name, "_k015_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_4, paste(folder, csv.name, "_k020_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_5, paste(folder, csv.name, "_k025_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_6, paste(folder, csv.name, "_k030_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_7, paste(folder, csv.name, "_k035_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_8, paste(folder, csv.name, "_k040_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_9, paste(folder, csv.name, "_k045_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_10, paste(folder, csv.name, "_k050_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_11, paste(folder,csv.name, "_k055_24hour.csv", sep = ""), 
            row.names = F)
  write.csv(twentyfour_hour_table_12, paste(folder, csv.name, "_k060_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_13, paste(folder, csv.name, "_k065_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_14, paste(folder, csv.name, "_k070_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_15, paste(folder, csv.name, "_k075_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_16, paste(folder, csv.name, "_k080_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_17, paste(folder, csv.name, "_k085_24hour.csv", sep = ""), 
            row.names = F)
  write.csv(twentyfour_hour_table_18, paste(folder, csv.name, "_k090_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_19, paste(folder, csv.name, "_k095_24hour.csv", sep = ""),
            row.names = F)
  write.csv(twentyfour_hour_table_20, paste(folder, csv.name, "_k100_24hour.csv", sep = ""),
            row.names = F)
}

# Call the function "twenty_four_hour" for each k1 value

csv.name <- "hybrid25000"
cluster.list <- clusters25000
twenty_four_hour(cluster.list = clusters25000,
                 csv.name = "hybrid25000")

##########################################################
# Cluster the ACOUSTIC SIGNATURES
##########################################################
myFiles <- list.files(path = folder, full.names=TRUE, 
                      pattern="*_24hour.csv$")
#myFiles <- myFiles[101:120]
myFilesShort <- list.files(path = folder, full.names=FALSE, pattern="*_24hour.csv$")
#myFilesShort <- myFilesShort[101:120]
k1values <- substr(myFilesShort,7,11)
k2values <- substr(myFilesShort, 14,16)
length <- length(myFiles)
length
site <- c(rep("Gympie  NP  ", 6), rep("WoondumNP", 6))
#dates <- unique(indices$rec.date)
dates <- c("30 July 2015", "31 July 2015", "1 Aug 2015",
           "31 Aug 2015", "1 Sept 2015", "4 Sept 2015")
dates <- rep(dates, 2)

# Read file contents of Summary Indices and collate
numberCol <- NULL
heights <- NULL
I3DD_values <- NULL

######################################################
# Use the hc.fit heights to calculate the I3DD values
######################################################
ref <- 0
for (i in 1:length(myFilesShort)) {
  Name <- myFiles[i]
  assign("fileContents", read.csv(Name))
  numberCol <- ncol(fileContents)
  dat <- fileContents[,1:numberCol]
  hc.fit <- hclust(dist(dat), method = "ward.D2")
  ###########  SAVING ROW 1 
  if (hc.fit$merge[1] < 0 & hc.fit$merge[12] < 0) {
    row1 <- c(abs(hc.fit$merge[1]),hc.fit$height[1],abs(hc.fit$merge[12]))
  }
  ###########  SAVING ROW 2
  if (hc.fit$merge[2]<0 & hc.fit$merge[13]<0) {
    row2 <- c(abs(hc.fit$merge[2]),hc.fit$height[2],abs(hc.fit$merge[13]))
  }
  if (hc.fit$merge[2]<0 & hc.fit$merge[13]==1) {
    row2 <- c(abs(hc.fit$merge[2]),hc.fit$height[2],row1)
  }
  ###########  SAVING ROW 3
  if (hc.fit$merge[3]<0 & hc.fit$merge[14]<0) {
    row3 <- c(abs(hc.fit$merge[3]),hc.fit$height[3],abs(hc.fit$merge[14]))
  }
  if (hc.fit$merge[3]<0 & hc.fit$merge[14]==1) {
    row3 <- c(abs(hc.fit$merge[3]),hc.fit$height[3],row1)
  }
  if (hc.fit$merge[3]<0 & hc.fit$merge[14]==2) {
    row3 <- c(abs(hc.fit$merge[3]),hc.fit$height[3],row2)
  }
  if (hc.fit$merge[3]==1 & hc.fit$merge[14]==2) {
    row3 <- c(row1,hc.fit$height[3],row2)
  }
  ###########  SAVING ROW 4
  #negative-negative
  if (hc.fit$merge[4]<0 & hc.fit$merge[15]<0) {
    row4 <- c(abs(hc.fit$merge[4]),hc.fit$height[4],abs(hc.fit$merge[15]))
  }
  #negative-positive
  if (hc.fit$merge[4]<0 & hc.fit$merge[15]==1) {
    row4 <- c(abs(hc.fit$merge[4]),hc.fit$height[4],row1)
  }
  if (hc.fit$merge[4]<0 & hc.fit$merge[15]==2) {
    row4 <- c(abs(hc.fit$merge[4]),hc.fit$height[4],row2)
  }
  if (hc.fit$merge[4]<0 & hc.fit$merge[15]==3) {
    row4 <- c(abs(hc.fit$merge[4]),hc.fit$height[4],row3)
  }
  #positive-positive
  if (hc.fit$merge[4]==1 & hc.fit$merge[15]==2) {
    row4 <- c(row1,hc.fit$height[4],row2)
  }
  if (hc.fit$merge[4]==1 & hc.fit$merge[15]==3) {
    row4 <- c(row1,hc.fit$height[4],row3)
  }
  if (hc.fit$merge[4]==2 & hc.fit$merge[15]==3) {
    row4 <- c(row2,hc.fit$height[4],row3)
  }
  ###########  SAVING ROW 5
  # negative-negative
  if (hc.fit$merge[5]<0 & hc.fit$merge[16]<0) {
    row5 <- c(abs(hc.fit$merge[5]),hc.fit$height[5],abs(hc.fit$merge[16]))
  }
  # negative-positive
  if (hc.fit$merge[5]<0 & hc.fit$merge[16]==1) {
    row5 <- c(abs(hc.fit$merge[5]),hc.fit$height[5],row1)
  }
  if (hc.fit$merge[5]<0 & hc.fit$merge[16]==2) {
    row5 <- c(abs(hc.fit$merge[5]),hc.fit$height[5],row2)
  }
  if (hc.fit$merge[5]<0 & hc.fit$merge[16]==3) {
    row5 <- c(abs(hc.fit$merge[5]),hc.fit$height[5],row3)
  }
  if (hc.fit$merge[5]<0 & hc.fit$merge[16]==4) {
    row5 <- c(abs(hc.fit$merge[5]),hc.fit$height[5],row4)
  }
  #positive-positive
  if (hc.fit$merge[5]==1 & hc.fit$merge[16]==2) {
    row5 <- c(row1,hc.fit$height[5],row2)
  }
  if (hc.fit$merge[5]==1 & hc.fit$merge[16]==3) {
    row5 <- c(row1,hc.fit$height[5],row3)
  }
  if (hc.fit$merge[5]==1 & hc.fit$merge[16]==4) {
    row5 <- c(row1,hc.fit$height[5],row4)
  }
  if (hc.fit$merge[5]==2 & hc.fit$merge[16]==3) {
    row5 <- c(row2,hc.fit$height[5],row3)
  }
  if (hc.fit$merge[5]==2 & hc.fit$merge[16]==4) {
    row5 <- c(row2,hc.fit$height[5],row4)
  }
  if (hc.fit$merge[5]==3 & hc.fit$merge[16]==4) {
    row5 <- c(row3,hc.fit$height[5],row4)
  }
  ###########  SAVING ROW 6
  #negative-negative
  if (hc.fit$merge[6]<0 & hc.fit$merge[17]<0) {
    row6 <- c(abs(hc.fit$merge[6]),hc.fit$height[6],abs(hc.fit$merge[17]))
  }
  #negative-positive
  if (hc.fit$merge[6]<0 & hc.fit$merge[17]==1) {
    row6 <- c(abs(hc.fit$merge[6]),hc.fit$height[6],row1)
  }
  if (hc.fit$merge[6]<0 & hc.fit$merge[17]==2) {
    row6 <- c(abs(hc.fit$merge[6]),hc.fit$height[6],row2)
  }
  if (hc.fit$merge[6] <0 & hc.fit$merge[17]==3) {
    row6 <- c(abs(hc.fit$merge[6]),hc.fit$height[6],row3)
  }
  if (hc.fit$merge[6]<0 & hc.fit$merge[17]==4) {
    row6 <- c(abs(hc.fit$merge[6]),hc.fit$height[6],row4)
  }
  if (hc.fit$merge[6]<0 & hc.fit$merge[17]==5) {
    row6 <- c(abs(hc.fit$merge[6]),hc.fit$height[6],row5)
  }
  #positive-positive
  if (hc.fit$merge[6]==1 & hc.fit$merge[17]==2) {
    row6 <- c(row1,hc.fit$height[6],row2)
  }
  if (hc.fit$merge[6]==1 & hc.fit$merge[17]==3) {
    row6 <- c(row1,hc.fit$height[6],row3)
  }
  if (hc.fit$merge[6]==1 & hc.fit$merge[17]==4) {
    row6 <- c(row1,hc.fit$height[6],row4)
  }
  if (hc.fit$merge[6]==1 & hc.fit$merge[17]==5) {
    row6 <- c(row1,hc.fit$height[6],row5)
  }
  if (hc.fit$merge[6]==2 & hc.fit$merge[17]==3) {
    row6 <- c(row2,hc.fit$height[6],row3)
  }
  if (hc.fit$merge[6]==2 & hc.fit$merge[17]==4) {
    row6 <- c(row2,hc.fit$height[6],row4)
  }
  if (hc.fit$merge[6]==2 & hc.fit$merge[17]==5) {
    row6 <- c(row2,hc.fit$height[6],row5)
  }
  if (hc.fit$merge[6]==3 & hc.fit$merge[17]==4) {
    row6 <- c(row3,hc.fit$height[6],row4)
  }
  if (hc.fit$merge[6]==3 & hc.fit$merge[17]==5) {
    row6 <- c(row3,hc.fit$height[6],row5)
  }
  if (hc.fit$merge[6]==4 & hc.fit$merge[17]==5) {
    row6 <- c(row4,hc.fit$height[6],row5)
  }
  ###########  SAVING ROW 7
  #negative-negative
  if (hc.fit$merge[7]<0 & hc.fit$merge[18]<0) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],abs(hc.fit$merge[18]))
  }
  #negative-positive
  if (hc.fit$merge[7]<0 & hc.fit$merge[18]==1) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],row1)
  }
  if (hc.fit$merge[7]<0 & hc.fit$merge[18]==2) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],row2)
  }
  if (hc.fit$merge[7] <0 & hc.fit$merge[18]==3) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],row3)
  }
  if (hc.fit$merge[7]<0 & hc.fit$merge[18]==4) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],row4)
  }
  if (hc.fit$merge[7]<0 & hc.fit$merge[18]==5) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],row5)
  }
  if (hc.fit$merge[7]<0 & hc.fit$merge[18]==6) {
    row7 <- c(abs(hc.fit$merge[7]),hc.fit$height[7],row6)
  }
  #postive-positive
  if (hc.fit$merge[7]==1 & hc.fit$merge[18]==2) {
    row7 <- c(row1,hc.fit$height[7],row2)
  }
  if (hc.fit$merge[7]==1 & hc.fit$merge[18]==3) {
    row7 <- c(row1,hc.fit$height[7],row3)
  }
  if (hc.fit$merge[7]==1 & hc.fit$merge[18]==4) {
    row7 <- c(row1,hc.fit$height[7],row4)
  }
  if (hc.fit$merge[7]==1 & hc.fit$merge[18]==5) {
    row7 <- c(row1,hc.fit$height[7],row5)
  }
  if (hc.fit$merge[7]==1 & hc.fit$merge[18]==6) {
    row7 <- c(row1,hc.fit$height[7],row6)
  }
  if (hc.fit$merge[7]==2 & hc.fit$merge[18]==3) {
    row7 <- c(row2,hc.fit$height[7],row3)
  }
  if (hc.fit$merge[7]==2 & hc.fit$merge[18]==4) {
    row7 <- c(row2,hc.fit$height[7],row4)
  }
  if (hc.fit$merge[7]==2 & hc.fit$merge[18]==5) {
    row7 <- c(row2,hc.fit$height[7],row5)
  }
  if (hc.fit$merge[7]==2 & hc.fit$merge[18]==6) {
    row7 <- c(row2,hc.fit$height[7],row6)
  }
  if (hc.fit$merge[7]==3 & hc.fit$merge[18]==4) {
    row7 <- c(row3,hc.fit$height[7],row4)
  }
  if (hc.fit$merge[7]==3 & hc.fit$merge[18]==5) {
    row7 <- c(row3,hc.fit$height[7],row5)
  }
  if (hc.fit$merge[7]==3 & hc.fit$merge[18]==6) {
    row7 <- c(row3,hc.fit$height[7],row6)
  }
  if (hc.fit$merge[7]==4 & hc.fit$merge[18]==5) {
    row7 <- c(row4,hc.fit$height[7],row5)
  }
  if (hc.fit$merge[7]==4 & hc.fit$merge[18]==6) {
    row7 <- c(row4,hc.fit$height[7],row6)
  }
  if (hc.fit$merge[7]==5 & hc.fit$merge[18]==6) {
    row7 <- c(row5,hc.fit$height[7],row6)
  }
  ###########  SAVING ROW 8
  #negative-negative
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]<0) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],abs(hc.fit$merge[19]))
  }
  #negative-positive
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]==1) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row1)
  }
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]==2) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row2)
  }
  if (hc.fit$merge[8] <0 & hc.fit$merge[19]==3) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row3)
  }
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]==4) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row4)
  }
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]==5) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row5)
  }
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]==6) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row6)
  }
  if (hc.fit$merge[8]<0 & hc.fit$merge[19]==7) {
    row8 <- c(abs(hc.fit$merge[8]),hc.fit$height[8],row7)
  }
  #postive-positive
  if (hc.fit$merge[8]==1 & hc.fit$merge[19]==2) {
    row8 <- c(row1,hc.fit$height[8],row2)
  }
  if (hc.fit$merge[8]==1 & hc.fit$merge[19]==3) {
    row8 <- c(row1,hc.fit$height[8],row3)
  }
  if (hc.fit$merge[8]==1 & hc.fit$merge[19]==4) {
    row8 <- c(row1,hc.fit$height[8],row4)
  }
  if (hc.fit$merge[8]==1 & hc.fit$merge[19]==5) {
    row8 <- c(row1,hc.fit$height[8],row5)
  }
  if (hc.fit$merge[8]==1 & hc.fit$merge[19]==6) {
    row8 <- c(row1,hc.fit$height[8],row6)
  }
  if (hc.fit$merge[8]==1 & hc.fit$merge[19]==7) {
    row8 <- c(row1,hc.fit$height[8],row7)
  }
  if (hc.fit$merge[8]==2 & hc.fit$merge[19]==3) {
    row8 <- c(row2,hc.fit$height[8],row3)
  }
  if (hc.fit$merge[8]==2 & hc.fit$merge[19]==4) {
    row8 <- c(row2,hc.fit$height[8],row4)
  }
  if (hc.fit$merge[8]==2 & hc.fit$merge[19]==5) {
    row8 <- c(row2,hc.fit$height[8],row5)
  }
  if (hc.fit$merge[8]==2 & hc.fit$merge[19]==6) {
    row8 <- c(row2,hc.fit$height[8],row6)
  }
  if (hc.fit$merge[8]==2 & hc.fit$merge[19]==7) {
    row8 <- c(row2,hc.fit$height[8],row7)
  }
  if (hc.fit$merge[8]==3 & hc.fit$merge[19]==4) {
    row8 <- c(row3,hc.fit$height[8],row4)
  }
  if (hc.fit$merge[8]==3 & hc.fit$merge[19]==5) {
    row8 <- c(row3,hc.fit$height[8],row5)
  }
  if (hc.fit$merge[8]==3 & hc.fit$merge[19]==6) {
    row8 <- c(row3,hc.fit$height[8],row6)
  }
  if (hc.fit$merge[8]==3 & hc.fit$merge[19]==7) {
    row8 <- c(row3,hc.fit$height[8],row7)
  }
  if (hc.fit$merge[8]==4 & hc.fit$merge[19]==5) {
    row8 <- c(row4,hc.fit$height[8],row5)
  }
  if (hc.fit$merge[8]==4 & hc.fit$merge[19]==6) {
    row8 <- c(row4,hc.fit$height[8],row6)
  }
  if (hc.fit$merge[8]==4 & hc.fit$merge[19]==7) {
    row8 <- c(row4,hc.fit$height[8],row7)
  }
  if (hc.fit$merge[8]==5 & hc.fit$merge[19]==6) {
    row8 <- c(row5,hc.fit$height[8],row6)
  }
  if (hc.fit$merge[8]==5 & hc.fit$merge[19]==7) {
    row8 <- c(row5,hc.fit$height[8],row7)
  }
  if (hc.fit$merge[8]==6 & hc.fit$merge[19]==7) {
    row8 <- c(row6,hc.fit$height[8],row7)
  }
  ###########  SAVING ROW 9
  #negative-negative
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]<0) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],abs(hc.fit$merge[20]))
  }
  #negative-positive
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==1) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row1)
  }
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==2) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row2)
  }
  if (hc.fit$merge[9] <0 & hc.fit$merge[20]==3) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row3)
  }
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==4) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row4)
  }
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==5) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row5)
  }
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==6) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row6)
  }
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==7) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]<0 & hc.fit$merge[20]==8) {
    row9 <- c(abs(hc.fit$merge[9]),hc.fit$height[9],row8)
  }
  #positive-positive
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==2) {
    row9 <- c(row1,hc.fit$height[9],row2)
  }
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==3) {
    row9 <- c(row1,hc.fit$height[9],row3)
  }
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==4) {
    row9 <- c(row1,hc.fit$height[9],row4)
  }
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==5) {
    row9 <- c(row1,hc.fit$height[9],row5)
  }
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==6) {
    row9 <- c(row1,hc.fit$height[9],row6)
  }
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==7) {
    row9 <- c(row1,hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]==1 & hc.fit$merge[20]==8) {
    row9 <- c(row1,hc.fit$height[9],row8)
  }
  if (hc.fit$merge[9]==2 & hc.fit$merge[20]==3) {
    row9 <- c(row2,hc.fit$height[9],row3)
  }
  if (hc.fit$merge[9]==2 & hc.fit$merge[20]==4) {
    row9 <- c(row2,hc.fit$height[9],row4)
  }
  if (hc.fit$merge[9]==2 & hc.fit$merge[20]==5) {
    row9 <- c(row2,hc.fit$height[9],row5)
  }
  if (hc.fit$merge[9]==2 & hc.fit$merge[20]==6) {
    row9 <- c(row2,hc.fit$height[9],row6)
  }
  if (hc.fit$merge[9]==2 & hc.fit$merge[20]==7) {
    row9 <- c(row2,hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]==2 & hc.fit$merge[20]==8) {
    row9 <- c(row2,hc.fit$height[9],row8)
  }
  if (hc.fit$merge[9]==3 & hc.fit$merge[20]==4) {
    row9 <- c(row3,hc.fit$height[9],row4)
  }
  if (hc.fit$merge[9]==3 & hc.fit$merge[20]==5) {
    row9 <- c(row3,hc.fit$height[9],row5)
  }
  if (hc.fit$merge[9]==3 & hc.fit$merge[20]==6) {
    row9 <- c(row3,hc.fit$height[9],row6)
  }
  if (hc.fit$merge[9]==3 & hc.fit$merge[20]==7) {
    row9 <- c(row3,hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]==3 & hc.fit$merge[20]==8) {
    row9 <- c(row3,hc.fit$height[9],row8)
  }
  if (hc.fit$merge[9]==4 & hc.fit$merge[20]==5) {
    row9 <- c(row4,hc.fit$height[9],row5)
  }
  if (hc.fit$merge[9]==4 & hc.fit$merge[20]==6) {
    row9 <- c(row4,hc.fit$height[9],row6)
  }
  if (hc.fit$merge[9]==4 & hc.fit$merge[20]==7) {
    row9 <- c(row4,hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]==4 & hc.fit$merge[20]==8) {
    row9 <- c(row4,hc.fit$height[9],row8)
  }
  if (hc.fit$merge[9]==5 & hc.fit$merge[20]==6) {
    row9 <- c(row5,hc.fit$height[9],row6)
  }
  if (hc.fit$merge[9]==5 & hc.fit$merge[20]==7) {
    row9 <- c(row5,hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]==5 & hc.fit$merge[20]==8) {
    row9 <- c(row5,hc.fit$height[9],row8)
  }
  if (hc.fit$merge[9]==6 & hc.fit$merge[20]==7) {
    row9 <- c(row6,hc.fit$height[9],row7)
  }
  if (hc.fit$merge[9]==6 & hc.fit$merge[20]==8) {
    row9 <- c(row6,hc.fit$height[9],row8)
  }
  if (hc.fit$merge[9]==7 & hc.fit$merge[20]==8) {
    row9 <- c(row7,hc.fit$height[9],row8)
  }
  ###########  SAVING ROW 10
  #negative-negative
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]<0) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],abs(hc.fit$merge[21]))
  }
  #negative-postive
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==1) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row1)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==2) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row2)
  }
  if (hc.fit$merge[10] <0 & hc.fit$merge[21]==3) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row3)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==4) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row4)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==5) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row5)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==6) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row6)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==7) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==8) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]<0 & hc.fit$merge[21]==9) {
    row10 <- c(abs(hc.fit$merge[10]),hc.fit$height[10],row9)
  }
  #positive-positive
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==2) {
    row10 <- c(row1,hc.fit$height[10],row2)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==3) {
    row10 <- c(row1,hc.fit$height[10],row3)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==4) {
    row10 <- c(row1,hc.fit$height[10],row4)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==5) {
    row10 <- c(row1,hc.fit$height[10],row5)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==6) {
    row10 <- c(row1,hc.fit$height[10],row6)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==7) {
    row10 <- c(row1,hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==8) {
    row10 <- c(row1,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==1 & hc.fit$merge[21]==9) {
    row10 <- c(row1,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==3) {
    row10 <- c(row2,hc.fit$height[10],row3)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==4) {
    row10 <- c(row2,hc.fit$height[10],row4)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==5) {
    row10 <- c(row2,hc.fit$height[10],row5)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==6) {
    row10 <- c(row2,hc.fit$height[10],row6)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==7) {
    row10 <- c(row2,hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==8) {
    row10 <- c(row2,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==2 & hc.fit$merge[21]==9) {
    row10 <- c(row2,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==3 & hc.fit$merge[21]==4) {
    row10 <- c(row3,hc.fit$height[10],row4)
  }
  if (hc.fit$merge[10]==3 & hc.fit$merge[21]==5) {
    row10 <- c(row3,hc.fit$height[10],row5)
  }
  if (hc.fit$merge[10]==3 & hc.fit$merge[21]==6) {
    row10 <- c(row3,hc.fit$height[10],row6)
  }
  if (hc.fit$merge[10]==3 & hc.fit$merge[21]==7) {
    row10 <- c(row3,hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]==3 & hc.fit$merge[21]==8) {
    row10 <- c(row3,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==3 & hc.fit$merge[21]==9) {
    row10 <- c(row3,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==4 & hc.fit$merge[21]==5) {
    row10 <- c(row4,hc.fit$height[10],row5)
  }
  if (hc.fit$merge[10]==4 & hc.fit$merge[21]==6) {
    row10 <- c(row4,hc.fit$height[10],row6)
  }
  if (hc.fit$merge[10]==4 & hc.fit$merge[21]==7) {
    row10 <- c(row4,hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]==4 & hc.fit$merge[21]==8) {
    row10 <- c(row4,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==4 & hc.fit$merge[21]==9) {
    row10 <- c(row4,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==5 & hc.fit$merge[21]==6) {
    row10 <- c(row5,hc.fit$height[10],row6)
  }
  if (hc.fit$merge[10]==5 & hc.fit$merge[21]==7) {
    row10 <- c(row5,hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]==5 & hc.fit$merge[21]==8) {
    row10 <- c(row5,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==5 & hc.fit$merge[21]==9) {
    row10 <- c(row5,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==6 & hc.fit$merge[21]==7) {
    row10 <- c(row6,hc.fit$height[10],row7)
  }
  if (hc.fit$merge[10]==6 & hc.fit$merge[21]==8) {
    row10 <- c(row6,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==6 & hc.fit$merge[21]==9) {
    row10 <- c(row6,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==7 & hc.fit$merge[21]==8) {
    row10 <- c(row7,hc.fit$height[10],row8)
  }
  if (hc.fit$merge[10]==7 & hc.fit$merge[21]==9) {
    row10 <- c(row7,hc.fit$height[10],row9)
  }
  if (hc.fit$merge[10]==8 & hc.fit$merge[21]==9) {
    row10 <- c(row8,hc.fit$height[10],row9)
  }
  ###########  SAVING ROW 11
  #negative-positive
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==1) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row1)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==2) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row2)
  }
  if (hc.fit$merge[11] <0 & hc.fit$merge[22]==3) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row3)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==4) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row4)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[21]==5) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row5)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[21]==6) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row6)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==7) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==8) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==9) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]<0 & hc.fit$merge[22]==10) {
    row11 <- c(abs(hc.fit$merge[11]),hc.fit$height[11],row10)
  }
  #positive-positive
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==2) {
    row11 <- c(row1,hc.fit$height[11],row2)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==3) {
    row11 <- c(row1,hc.fit$height[11],row3)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==4) {
    row11 <- c(row1,hc.fit$height[11],row4)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==5) {
    row11 <- c(row1,hc.fit$height[11],row5)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==6) {
    row11 <- c(row1,hc.fit$height[11],row6)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==7) {
    row11 <- c(row1,hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==8) {
    row11 <- c(row1,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==9) {
    row11 <- c(row1,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==1 & hc.fit$merge[22]==10) {
    row11 <- c(row1,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==3) {
    row11 <- c(row2,hc.fit$height[11],row3)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==4) {
    row11 <- c(row2,hc.fit$height[11],row4)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==5) {
    row11 <- c(row2,hc.fit$height[11],row5)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==6) {
    row11 <- c(row2,hc.fit$height[11],row6)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==7) {
    row11 <- c(row2,hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==8) {
    row11 <- c(row2,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==9) {
    row11 <- c(row2,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==2 & hc.fit$merge[22]==10) {
    row11 <- c(row2,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==4) {
    row11 <- c(row3,hc.fit$height[11],row4)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==5) {
    row11 <- c(row3,hc.fit$height[11],row5)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==6) {
    row11 <- c(row3,hc.fit$height[11],row6)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==7) {
    row11 <- c(row3,hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==8) {
    row11 <- c(row3,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==9) {
    row11 <- c(row3,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==3 & hc.fit$merge[22]==10) {
    row11 <- c(row3,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==4 & hc.fit$merge[22]==5) {
    row11 <- c(row4,hc.fit$height[11],row5)
  }
  if (hc.fit$merge[11]==4 & hc.fit$merge[22]==6) {
    row11 <- c(row4,hc.fit$height[11],row6)
  }
  if (hc.fit$merge[11]==4 & hc.fit$merge[22]==7) {
    row11 <- c(row4,hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]==4 & hc.fit$merge[22]==8) {
    row11 <- c(row4,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==4 & hc.fit$merge[22]==9) {
    row11 <- c(row4,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==4 & hc.fit$merge[22]==10) {
    row11 <- c(row4,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==5 & hc.fit$merge[22]==6) {
    row11 <- c(row5,hc.fit$height[11],row6)
  }
  if (hc.fit$merge[11]==5 & hc.fit$merge[22]==7) {
    row11 <- c(row5,hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]==5 & hc.fit$merge[22]==8) {
    row11 <- c(row5,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==5 & hc.fit$merge[22]==9) {
    row11 <- c(row5,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==5 & hc.fit$merge[22]==10) {
    row11 <- c(row5,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==6 & hc.fit$merge[22]==7) {
    row11 <- c(row6,hc.fit$height[11],row7)
  }
  if (hc.fit$merge[11]==6 & hc.fit$merge[22]==8) {
    row11 <- c(row6,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==6 & hc.fit$merge[22]==9) {
    row11 <- c(row6,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==6 & hc.fit$merge[22]==10) {
    row11 <- c(row6,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==7 & hc.fit$merge[22]==8) {
    row11 <- c(row7,hc.fit$height[11],row8)
  }
  if (hc.fit$merge[11]==7 & hc.fit$merge[22]==9) {
    row11 <- c(row7,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==7 & hc.fit$merge[22]==10) {
    row11 <- c(row7,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==8 & hc.fit$merge[22]==9) {
    row11 <- c(row8,hc.fit$height[11],row9)
  }
  if (hc.fit$merge[11]==8 & hc.fit$merge[22]==10) {
    row11 <- c(row8,hc.fit$height[11],row10)
  }
  if (hc.fit$merge[11]==9 & hc.fit$merge[22]==10) {
    row11 <- c(row9,hc.fit$height[11],row10)
  }
  all.heights <- c(row11[2],row11[4],row11[6],row11[8],
                   row11[10],row11[12],row11[14],row11[16],
                   row11[18],row11[20],row11[22])
  labels.in.order <- c(row11[1],row11[3],row11[5],row11[7],row11[9],
                       row11[11],row11[13],row11[15],row11[17],
                       row11[19],row11[21],row11[23])
  # Heights between days 1, 2 and 3 (Group 1)
  whichA <- which(labels.in.order==1|labels.in.order==2|labels.in.order==3)
  h <- NULL
  for (j in whichA[1]:(whichA[2]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsA <- max(h)
  h <- NULL
  for (j in whichA[2]:(whichA[3]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsB <- max(h)
  heights1 <- c(heightsA,heightsB)
  
  # Heights between days 4, 5 and 6 (Group 2)
  whichB <- which(labels.in.order==4|labels.in.order==5|labels.in.order==6)
  h <- NULL
  for (j in whichB[1]:(whichB[2]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsA <- max(h)
  h <- NULL
  for (j in whichB[2]:(whichB[3]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsB <- max(h)
  heights2 <- c(heightsA,heightsB)
  
  # Heights between days 7, 8 and 9 (Group 3)
  whichC <- which(labels.in.order==7|labels.in.order==8|labels.in.order==9)
  h <- NULL
  for (j in whichC[1]:(whichC[2]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsA <- max(h)
  h <- NULL
  for (j in whichC[2]:(whichC[3]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsB <- max(h)
  heights3 <- c(heightsA,heightsB)
  
  # Heights between days 10, 11 and 12 (Group 4)
  whichD <- which(labels.in.order==10|labels.in.order==11|labels.in.order==12)
  h <- NULL
  for (j in whichD[1]:(whichD[2]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsA <- max(h)
  h <- NULL
  for (j in whichD[2]:(whichD[3]-1)) {
    h <- c(h,all.heights[j])  
  }
  heightsB <- max(h)
  heights4 <- c(heightsA,heightsB)
  
  eight.heights.total <- sum(heights1,heights2,heights3,heights4)
  I3DD.separation <- round(((eight.heights.total/2)/max(all.heights)), 3)
  ref <- ref + 1
  values <- c(k1values[ref], k2values[ref], I3DD.separation)
  I3DD_values <- rbind(I3DD_values, values)
  folder <- "C:/plos-visualization-paper/plots/"
  png(paste(folder, myFilesShort[i],"Method wardD2.png", sep = ""), width=1110,
      height =1000)
  par(oma=c(7,3,3,3))
  plot(hc.fit, cex=2.5, main = paste(myFilesShort[i]), sub="", 
       xlab = "hclust(method = ward.D2)",
       xaxt="n", yaxt = "n", cex.lab=2, ylab="", 
       cex.main=2.2, lwd=3)
  mtext(side = 2, "Heights   ",
        cex = 2, line = -2, adj = 1)
  mtext(side = 3, paste("I3DD Separation","=", I3DD.separation, sep = " "),
        cex = 2.2, line = -5)
  heightss <- hc.fit$height
  axis(side = 4, at=c(round(heightss[2],0),round(heightss[4],0),round(heightss[6],0),
                      round(heightss[8],0),round(heightss[10],0)), 
       lwd=2,las=1, cex.axis=1.8)
  axis(side = 2, at=c(round(heightss[1],0),round(heightss[3],0),round(heightss[5],0),
                      round(heightss[7],0),round(heightss[9],0),round(heightss[11],0)), 
       lwd=2,las=1, cex.axis=1.8)
  mtext(side = 1, line = 5.5, adj=1, cex=1.3, paste("1 , 2 , 3 ", site[1], dates[1], dates[2], dates[3], 
                                                    "4 , 5 , 6 ", site[1], dates[4], dates[5], dates[6], 
                                                    sep = "    ")) 
  mtext(side = 1, line = 7, adj=1, cex=1.3, paste("7 , 8 , 9 ", site[7], dates[1], dates[2], dates[3], 
                                                  "10,11,12", site[7], dates[4], dates[5], dates[6], 
                                                  sep = "    "))
  mtext(side = 1, line = 8.5, adj=1, cex=1.2, expression(italic(Twelve ~days)))
  mtext(side = 1, line = 10, adj=1, cex=0.8, expression(italic(Indices:~BGN ~SNR ~ACT ~EVN ~HFC ~MFC ~LFC ~ACI ~EAS ~EPS ~ECV ~CLC)))
  mtext(side = 3, line = -0.8, cex=2, paste("heights: ", round(heightss[11],0), ",",
                                            round(heightss[10],0), ", ", round(heightss[9],0), ", ", 
                                            round(heightss[8],0), ", ",  round(heightss[7],0), ", ", 
                                            round(heightss[6],0), ", ", round(heightss[5],0), ", ",
                                            round(heightss[4],0), ", ", round(heightss[3],0), ", ", 
                                            round(heightss[2],0), ", ", round(heightss[1],0), sep = ""))
  heights <- rbind(heights, heightss)
  dev.off()
}
colnames(I3DD_values) <- c("k1", "k2", "I3DD")

folder <- "C:/plos-visualization-paper/results/"
write.csv(I3DD_values, paste(folder,"I3DD_values_", csv.name,
                             ".csv",sep=""), row.names = F)