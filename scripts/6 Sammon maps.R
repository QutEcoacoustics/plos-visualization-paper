# Title:  Sammon Map - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code produces 

# File and requirements (the normalised summary indices) 
# Note: The file was generated in normalisation code and should already
# be in the results folder
# C:/plos-visualization-paper/results/Gympie_woondum_normalised_summary_indices.RData

# Time requirements: about xxx minutes

# Package requirements
# cluster, MASS and plotrix

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sammon map 1----------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())

# load normalised summary indices
load("C:/plos-visualization-paper/results/Gympie_woondum_normalised_summary_indices.RData")
indices_norm_summary <- complete_DF
rm(complete_DF, F)

# choose a specific k1 and k2 hybrid cluster combination
k1_value <- 25000
k2_value <- 60
column <- k2_value/5

# load "hclust_clusters_(insert k1_value)" dataframe
load("C:/plos-visualization-paper/results/hclust_clusters_25000.RData")
# load the cluster list 
cluster.list <- get(paste("hclust_clusters_",k1_value, sep=""), envir=globalenv())[,column]
rm(k1_value, k2_value, column, hclust_clusters_25000)

# concatenate the clusterlist and the normalised indices
indices_norm_summary <- cbind(cluster.list, indices_norm_summary)

# list the cluster numbers and sort
cluster_num <- unique(indices_norm_summary$cluster.list)
cluster_num <- sort(cluster_num)

# The 'cluster' package is required for the clara function
# Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K.
# (2015). R package 'cluster' Cluster Analysis Basics and Extensions 
# Retrieved from https://cran.r-project.org/web/packages/cluster/
library(cluster)
# determine the medoid of each cluster
medoids <- NULL
for(i in 1:length(cluster_num)) {
  a <- which(indices_norm_summary$cluster.list==i)
  clust <- indices_norm_summary[a,2:ncol(indices_norm_summary)]
  medo <- clara(clust,1)$medoids
  medoids <- rbind(medoids, medo)
}
rownames(medoids) <- as.character(as.numeric(cluster_num))
rm(medo, a, i)

medoids <- data.frame(medoids)
medoids <- cbind(c(1:60), medoids)
colnames(medoids) <- c("clust", "BGN","SNR","ACT",
                       "EVN", "HFC", "MFC", "LFC",
                       "ACI", "EAS", "EPS", "ECV",
                       "CLC")

distances <- as.matrix(dist(medoids[,2:13]))
# 'MASS' package needed for sammon function
# Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with
#  S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
library(MASS)

clusters.sam <- sammon(distances, k=2)
rm(distances)

# detmine the count of each cluster and the number of the
# NA values
clust_sizes <- unname(table(cluster.list))

clusters <- NULL
clusters$clusters <- cluster_num
clusters$points1 <- clusters.sam$points[,1]
clusters$points2 <- clusters.sam$points[,2]
clusters$size <- clust_sizes[1:60]
clusters$colours <- NULL
clusters <- data.frame(clusters)
clusters$radius <- sqrt(clusters$size)

# colours for each class, these are from a colourblind pallet
insects <- "#F0E442"
rain <- "#0072B2"
wind <- "#56B4E9"
birds <- "#009E73"
cicadas <- "#E69F00"
quiet <- "#999999"
planes <- "#CC79A7"

clusters$colours <- "---"
clusters$border <- "---"
#clusters$colours[1] <- insects
# set the circle and border colours
clusters[1, 6:7]  <-  c(insects, insects)
clusters[2, 6:7]  <-  c(rain, birds)
clusters[3, 6:7]  <-  c(birds, birds)
clusters[4, 6:7]  <-  c(insects, birds)
clusters[5, 6:7]  <-  c(quiet, quiet)
clusters[6, 6:7]  <-  c(quiet, quiet)
clusters[7, 6:7]  <-  c(cicadas, birds)
clusters[8, 6:7]  <-  c(cicadas, birds)
clusters[9, 6:7]  <-  c(wind, wind)
clusters[10, 6:7]  <-  c(rain, rain)
clusters[11, 6:7]  <-  c(birds, birds)
clusters[12, 6:7]  <-  c(cicadas, cicadas)
clusters[13, 6:7]  <-  c(quiet, quiet)
clusters[14, 6:7]  <-  c(birds, birds)
clusters[15, 6:7]  <-  c(birds, birds)
clusters[16, 6:7]  <-  c(cicadas, cicadas)
clusters[17, 6:7]  <-  c(rain, insects)
clusters[18, 6:7]  <-  c(rain, rain)
clusters[19, 6:7]  <-  c(wind, wind)
clusters[20, 6:7]  <-  c(wind, wind)
clusters[21, 6:7]  <-  c(rain, rain)
clusters[22, 6:7]  <-  c(insects, birds)
clusters[23, 6:7]  <-  c(planes, planes)
clusters[24, 6:7]  <-  c(wind, cicadas)
clusters[25, 6:7]  <-  c(wind, wind)
clusters[26, 6:7]  <-  c(insects, wind)
clusters[27, 6:7]  <-  c(insects, insects)
clusters[28, 6:7]  <-  c(birds, insects)
clusters[29, 6:7]  <-  c(insects, insects)
clusters[30, 6:7]  <-  c(wind, quiet)
clusters[31, 6:7]  <-  c(quiet, quiet)
clusters[32, 6:7]  <-  c(cicadas, cicadas)
clusters[33, 6:7]  <-  c(birds, birds)
clusters[34, 6:7]  <-  c(cicadas, cicadas)
clusters[35, 6:7]  <-  c(quiet, quiet)
clusters[36, 6:7]  <-  c(quiet, planes)
clusters[37, 6:7]  <-  c(birds, birds)
clusters[38, 6:7]  <-  c(quiet, quiet)
clusters[39, 6:7]  <-  c(birds, planes)
clusters[40, 6:7]  <-  c(wind, birds)
clusters[41, 6:7]  <-  c(quiet, quiet)
clusters[42, 6:7]  <-  c(wind, wind)
clusters[43, 6:7]  <-  c(birds, birds)
clusters[44, 6:7]  <-  c(cicadas, cicadas)
clusters[45, 6:7]  <-  c(wind, planes)
clusters[46, 6:7]  <-  c(wind, wind)
clusters[47, 6:7]  <-  c(wind, wind)
clusters[48, 6:7]  <-  c(cicadas, cicadas)
clusters[49, 6:7]  <-  c(planes, planes)
clusters[50, 6:7]  <-  c(quiet, insects)
clusters[51, 6:7]  <-  c(wind, wind)
clusters[52, 6:7]  <-  c(wind, wind) 
clusters[53, 6:7]  <-  c(quiet, quiet)
clusters[54, 6:7]  <-  c(rain, birds)
clusters[55, 6:7]  <-  c(quiet, quiet)
clusters[56, 6:7]  <-  c(wind, wind)
clusters[57, 6:7]  <-  c(birds, wind) 
clusters[58, 6:7]  <-  c(birds, birds)
clusters[59, 6:7]  <-  c(rain, rain) 
clusters[60, 6:7]  <-  c(rain, birds) 
clusters <- clusters[order(-clusters$size),]

# set the legend colour and names
leg_col <- as.character(c(rain, birds, cicadas, wind, planes, quiet, insects))
leg_names <- c("rain", "birds", "cicadas", "wind", "planes", "quiet","orthoptera")

# 'plotrix' package needed for draw.circle function
# Lemon J. Plotrix: a package in the red light district of R. R-News. 2006;6(4):8-12.
library(plotrix)
max <- 0.0009 # this is a scale factor for the plots

tiff("C:/plos-visualization-paper/plots/Sammon plot_1.tiff", 
     width = 2250, height = 1500, units = 'px', res = 300)
par(mar=c(2.5, 2.8, 1, 0.4), 
    cex = 1, cex.axis = 1, cex.main = 2.4)
plot(clusters$points1, 
     clusters$points2, type = "n",
     main = "Sammon map - sixty clusters",
     xlab = "", ylab = "", mgp =c(2,0.5,0),
     xlim = c((min(clusters$points1)-0.05),
              (max(clusters$points1)+0.25)),
     ylim = c((min(clusters$points2)-0.11),
              (max(clusters$points2)+0.13)),
     cex.axis=1, cex.lab=0.6, las=1, cex.main=1,
     bg = "transparent")
mtext(side=2, "y", las=1, cex = 1.2, line = 2.1, padj=1)
mtext(side=1, "x", las=1, cex = 1.2, line = 1.5)
for(i in 1:nrow(clusters)) {
  draw.circle(clusters$points1[i],
              clusters$points2[i], 
              radius = max*clusters$radius[i],
              col = clusters$colours[i],
              border = clusters$border[i],
              lwd = 6)
}
# plot the x and y axis to form four quadrants
abline(h = 0, col = "gray40", lwd = 1)
abline(v = 0, col = "gray40", lwd = 1)
# plot the cluster numbers
text(clusters$points1, clusters$points2, 
     labels = as.character(clusters$clusters), cex = 1)
# plot the plot legend
a <-legend("topright", title="Classes", 
           col = leg_col, bty = "n", 
           cex=1.1, leg_names , y.intersp = 0.85) 
for(j in 1:length(a$text$x)) {
  draw.circle(a$text$x[j]-0.06, a$text$y[j]-0.005, 
              radius = 0.035,
              col = leg_col[j],
              border = "white")
}
# add family to fonts list use windowsFonts() to check current
windowsFonts(A = windowsFont("Times New Roman"))
text(x = 1.1, y = 1.1, "I", cex = 1, family="A", font = 2)
text(x = -1.6, y = 1.1, "II", cex = 1, family="A", font = 2)
text(x = -1.6, y = -1.05, "III", cex = 1, family="A", font = 2)
text(x = 1.1, y = -1.05, "IV", cex = 1, family="A", font = 2)
dev.off()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Sammon map 2-----------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# calculate the radius by finding the distances from each medoid 
# to each point in the cluster and then calculating the 90th 
# percentile

# resort the cluster dataframe to order the cluster list
clusters <- clusters[order(clusters$clusters),]

# WARNING the calculation of the radii takes 12 minutes
statistics <- NULL
statistics <- data.frame(statistics)
for(j in cluster_num) {
  clust_num <- cluster_num[j]
  a <- which(cluster.list==clust_num)
  temp_data <- indices_norm_summary[a,1:13]
  data_frame <- data.frame(BGN=0,
                           SNR=0,
                           ACT=0,
                           EVN=0,
                           HFC=0,
                           MFC=0,
                           LFC=0,
                           ACI=0,
                           EAS=0,
                           EPS=0,
                           ECS=0,
                           CCL=0)
  # find the distance from each cluster medoid to each point
  distances <- NULL
  data_frame[1, 1:12] <- medoids[j,2:13]
  for(i in 1:nrow(temp_data)) {
    data_frame[2, 1:12] <- temp_data[i, 2:13]
    d <- dist(data_frame)
    distances <- c(distances, d)
  }
  prob <- 0.90    
  statistics[j,1] <- quantile(distances,pvec)
  print(j) # of 60
}
rm(temp_data)
clusters <- clusters[,1:7]
clusters$radius2 <- NULL
clusters$radius2 <- statistics$V1
max <- 0.16
clusters <- clusters[order(-clusters$radius2),]

tiff("C:/plos-visualization-paper/plots/Sammon_map_diameters.tiff", 
     width = 2250, height = 1500, units = 'px', res = 300)
par(mar=c(2.5, 2.8, 1, 0.4), 
    cex = 1, cex.axis = 1, cex.main = 2.4)
plot(clusters$points1, 
     clusters$points2, type = "n",
     main = "Sammon map of cluster medoids",
     xlab = "",ylab = "", mgp =c(2,0.5,0),
     xlim = c((min(clusters$points1)-0.05),
              (max(clusters$points1)+0.25)),
     ylim = c((min(clusters$points2)-0.11),
              (max(clusters$points2)+0.13)),
     cex.axis=1, cex.lab=0.6, las=1, cex.main=1,
     bg = "transparent")
mtext(side=2, "y", las=1, cex = 1.2, line = 2.1, padj=1)
mtext(side=1, "x", las=1, cex = 1.2, line = 1.5)
for(i in 1:nrow(clusters)) {
  draw.circle(clusters$points1[i],
              clusters$points2[i], 
              radius = max*clusters$radius2[i],
              col = clusters$colours[i],
              border = clusters$border[i],
              lwd = 6)
}
# plot the x and y axis to form four quadrants
abline(h = 0, col = "gray40", lwd = 1)
abline(v = 0, col = "gray40", lwd = 1)
# plot the cluster numbers
text(clusters$points1, clusters$points2, 
     labels = as.character(clusters$clusters), cex = 1)
# plot the plot legend
a <-legend("topright", title="Classes", 
           col = leg_col, bty = "n", 
           cex=1.1, leg_names , y.intersp = 0.85) 
for(j in 1:length(a$text$x)) {
  draw.circle(a$text$x[j]-0.06, a$text$y[j]-0.005, 
              radius = 0.035,
              col = leg_col[j],
              border = "white")
}
# add family to fonts list use windowsFonts() to check current
windowsFonts(A = windowsFont("Times New Roman"))
text(x = 1.75, y = 1.1, "I", cex = 1, family="A", font = 2)
text(x = -1.6, y = 1.1, "II", cex = 1, family="A", font = 2)
text(x = -1.6, y = -1.05, "III", cex = 1, family="A", font = 2)
text(x = 1.75, y = -1.05, "IV", cex = 1, family="A", font = 2)
dev.off()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Combined Sammon map ------------------------------
# top proportional to the number of minutes in cluster
# bottom proportional to the diameter of the cluster
# also see sammon map showing diameters.R
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dev.off()

tiff("C:/plos-visualization-paper/plots/Combined_Sammon_maps.tiff", width = (0.8459259*2025), 
     height = (0.8459259*2362), units = 'px', res = 300)
par(mfrow = c(2,1), mar=c(1.5, 2, 1, 0.4), 
    cex = (0.8459259*1), cex.axis = (0.8459259*1), 
    cex.main = (0.8459259*2.4))
# 1st sammon plot
max <- 0.0009
clusters <- clusters[order(-clusters$size),]
plot(clusters$points1, 
     clusters$points2, type = "n",
     main = "Sammon map of cluster medoids",
     xlab = "x",ylab = "", mgp =c(2,0.5,0),
     xlim = c((min(clusters$points1)-0.05),
              (max(clusters$points1)+0.6)),
     ylim = c((min(clusters$points2)-0.11),
              (max(clusters$points2)+0.13)),
     cex.axis=1, cex.lab=0.6, las=1, cex.main=1,
     bg = "transparent")
mtext(side=2, "y", las=1, cex = 3, line = 3.5)
#abline(h = seq(-10, 10, 0.1), col = "lightgray", lty = 3)
#abline(v = seq(-10, 10, 0.1), col = "lightgray", lty = 3)
for(i in 1:nrow(clusters)) {
  draw.circle(clusters$points1[i],
              clusters$points2[i], 
              radius = max*clusters$radius[i],
              col = clusters$colours[i],
              border = clusters$border[i],
              lwd = (0.8459259*6))
}
# plot the x and y axis to form four quadrants
abline(h = 0, col = "gray40", lwd = 1)
abline(v = 0, col = "gray40", lwd = 1)
# plot the cluster numbers
text(clusters$points1, clusters$points2, 
     labels = as.character(clusters$clusters), cex = 1)
# plot the plot legend
a <-legend("topright", title="Classes", 
           col = leg_col, bty = "n", 
           cex=1.1, leg_names , y.intersp = 0.85) 
for(j in 1:length(a$text$x)) {
  draw.circle(a$text$x[j]-0.06, a$text$y[j]-0.005, 
              radius = 0.035,
              col = leg_col[j],
              border = "white")
}
# add family to fonts list use windowsFonts() to check current
windowsFonts(A = windowsFont("Times New Roman"))
text(x = 2, y = 1.1, "I", cex = 1, family="A", font = 2)
text(x = -1.6, y = 1.1, "II", cex = 1, family="A", font = 2)
text(x = -1.6, y = -1.05, "III", cex = 1, family="A", font = 2)
text(x = 2, y = -1.05, "IV", cex = 1, family="A", font = 2)
mtext(at = 1.29, line = 0.5, side = 2, "a.", cex=1.2, las=1)

# 2nd sammon plot
clusters <- clusters[order(-clusters$radius2),]
max <- 0.16
plot(clusters$points1, 
     clusters$points2, type = "n",
     main = "Sammon map of cluster medoids",
     xlab = "x",ylab = "", mgp =c(2,0.5,0),
     xlim = c((min(clusters$points1)-0.05),
              (max(clusters$points1)+0.6)),
     ylim = c((min(clusters$points2)-0.11),
              (max(clusters$points2)+0.13)),
     cex.axis=1, cex.lab=0.6, las=1, cex.main=1,
     bg = "transparent")
mtext(side=2, "y", las=1, cex = 3, line = 3.5)
#abline(h = seq(-10, 10, 0.1), col = "lightgray", lty = 3)
#abline(v = seq(-10, 10, 0.1), col = "lightgray", lty = 3)
for(i in 1:nrow(clusters)) {
  draw.circle(x = clusters$points1[i],
              y = clusters$points2[i], 
              radius = max*clusters$radius2[i],
              col = clusters$colours[i],
              border = clusters$border[i],
              lwd = (0.8459259*6))
}
# plot the x and y axis to form four quadrants
abline(h = 0, col = "gray40", lwd = 1)
abline(v = 0, col = "gray40", lwd = 1)
# plot the cluster numbers
text(clusters$points1, clusters$points2, 
     labels = as.character(clusters$clusters), cex = 1)
# plot the plot legend
a <-legend("topright", title="Classes", 
           col = leg_col, bty = "n", 
           cex=1.1, leg_names , y.intersp = 0.85) 
for(j in 1:length(a$text$x)) {
  draw.circle(a$text$x[j]-0.06, a$text$y[j]-0.005, 
              radius = 0.035,
              col = leg_col[j],
              border = "white")
}
# add family to fonts list use windowsFonts() to check current
windowsFonts(A = windowsFont("Times New Roman"))
text(x = 2, y = 1.1, "I", cex = 1, family="A", font = 2)
text(x = -1.6, y = 1.1, "II", cex = 1, family="A", font = 2)
text(x = -1.6, y = -1.05, "III", cex = 1, family="A", font = 2)
text(x = 2, y = -1.05, "IV", cex = 1, family="A", font = 2)

mtext(at = 1.29, line = 0.5, side = 2, "b.", cex=1.2, las=1)

dev.off()
