# Title:  Radar Plots - Plos One paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y.F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Note: This code should only be run after 3 Hybrid clustering.R

# Description:  This code generates the two versions of radar plots
# displaying the cluster medoids using a slight alteration of the 
# radarchart function from the fmsb R package 

# Note: This file will need to be loaded manually
# it is available from https://github.com/QutEcoacoustics/plos-visualization-paper
# C:/plos-visualization-paper/scripts/radarPlot.R

# File and folder requirements
# These will be loaded automatically
# C:/plos-visualization-paper/data/gympieclusterlist
# C:/plos-visualization-paper/data/woondumclusterlist
# C:/plos-visualization-paper/results/hclust_clusters_25000.RData
# C:/plos-visualization-paper/plots

# Time requirements: less than 30 seconds

# Package requirements: cluster, fmsb

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Function and data sourcing for Radar Plots ---------------------------------
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())
start_time <- paste(Sys.time())

# make folder for plots if it does not exist
f <- paste0("C:/plos-visualization-paper/plots/")
if (!dir.exists(f)) {
  dir.create("C:/plos-visualization-paper/plots/")
}

# source the radarPlot function
# this is an adaption of the radarchart function in fmsb
# Nakazawa, M. (2015). 'fmsb' (R package) Version 0.5.2 
# Retrieved from https://cran.r-project.org/web/packages/fmsb/fmsb.pdf
# see inside code file for comments on how and why the code was adapted
source("C:/plos-visualization-paper/scripts/radarPlot.R")

# load normalised summary indices
load("C:/plos-visualization-paper/results/Gympie_woondum_normalised_summary_indices.RData")
indices_norm_summary <- complete_DF
rm(complete_DF, F)

# choose a specific k1 and k2 hybrid cluster combination
k1_value <- 25000
k2_value <- 60
column <- k2_value/5

file_name <- paste("C:/plos-visualization-paper/results/hclust_clusters_",
                   k1_value, ".RData", sep = "")
file_name_short <- paste("hclust_clusters_",k1_value, sep = "")

# load "hclust_clusters_(insert k1_value)" dataframe
load(file_name)

# Load and read the cluster list (if necessary)
u <- "https://data.researchdatafinder.qut.edu.au/dataset/62de1856-d030-423b-9ada-0b16eb06c0ba/resource/7a70163b-323b-4c30-aaf3-e19e934b328d/download/gympieclusterlist.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/", name,sep="")
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

# concatenate the clusterlist and the normalised indices
indices_norm_summary <- cbind(cluster_list, indices_norm_summary)

# list the cluster numbers and sort
cluster_num <- unique(indices_norm_summary$cluster_list)
cluster_num <- sort(cluster_num)

packages <- c("cluster", "fmsb")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# The 'cluster' package is required for the clara function
# Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K.
# (2015). R package 'cluster' Cluster Analysis Basics and Extensions 
# Retrieved from https://cran.r-project.org/web/packages/cluster/
library(cluster)
# determine the medoid of each cluster
medoids <- NULL
for(i in 1:length(cluster_num)) {
  a <- which(indices_norm_summary$cluster_list==i)
  clust <- indices_norm_summary[a,2:ncol(indices_norm_summary)]
  medo <- clara(clust,1)$medoids
  medoids <- rbind(medoids, medo)
}
rownames(medoids) <- as.character(as.numeric(cluster_num))

medoids <- data.frame(medoids)
medoids <- cbind(c(1:60), medoids)
colnames(medoids) <- c("clust", "BGN","SNR","ACT",
                    "EVN", "HFC", "MFC", "LFC",
                    "ACI", "EAS", "EPS", "ECV",
                    "CLC")

# Install the packages that have not been previously installed
# The 'fmsb' package is required for the radarplot function
library(fmsb) #Functions for Medical Statistics Book with some Demographic Data

# Six clusters are selected for ribbon plot
# one from each acoustic class except for insects
quiet5 <- 5
birds11 <- 11
rain59 <- 59
wind42  <- 42
cicadas48 <- 48
planes49 <- 49

insects_col <- "#F0E442"
rain_col <- "#0072B2"
wind_col <- "#56B4E9"
birds_col <- "#009E73"
cicadas_col <- "#E69F00"
quiet_col <- "#999999"
planes_col <- "#CC79A7"

colours <- c(quiet_col, birds_col, rain_col,
             wind_col, cicadas_col, planes_col) 

all <- c("quiet5", "birds11", "rain59", 
         "wind42", "cicadas48", "planes49")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Radar plots ----------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tiff("C:/plos-visualization-paper/plots/Radar_plots.tiff", 
     width = 2025, height = 1350, units = 'px', res = 300)
ref <- 1 
par(mfrow=c(2,3), xpd=NA, #decrease default margin
    mgp = c(0, 0.2, 0), cex = 0.6, oma = c(0, 0, 0, 0)) 
radarPlot(rbind(rep(1,60), rep(0,60), medoids[5,-1]), 
           pfcol=colours[ref], seg = 5, vlcex = 1.6, 
           axistype=0, centerzero = TRUE, plwd = 1.5, 
           pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
mtext("a. QUIET (Cluster 5)", side = 3, cex = 1.1, line = -0.1, adj=0)
#text(x = -0.9, y = 1.26, paste("Cluster 5"), cex = 1.6)
ref <- ref + 1
radarPlot(rbind(rep(1,60), rep(0,60), medoids[11,-1]), 
           pfcol=colours[ref], seg = 5, vlcex = 1.6, 
           axistype=0, centerzero = TRUE, plwd = 1.5, 
           pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
mtext("b. BIRDS (Cluster 11)", side = 3, cex = 1.1, line = -0.1, adj=0)
ref <- ref + 1
radarPlot(rbind(rep(1,60), rep(0,60), medoids[59,-1]), 
           pfcol=colours[ref], seg = 5, vlcex = 1.6, 
           axistype=0, centerzero = TRUE, plwd = 1.5, 
           pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
mtext("c. RAIN (Cluster 59)", side = 3, cex = 1.1, line = -0.1, adj=0)
ref <- ref + 1
radarPlot(rbind(rep(1,60), rep(0,60), medoids[42,-1]), 
           pfcol=colours[ref], seg = 5, vlcex = 1.6, 
           axistype=0, centerzero = TRUE, plwd = 1.5, 
           pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
mtext("d. WIND (Cluster 42)", side = 3, cex = 1.1, line = -0.1, adj=0)
ref <- ref + 1
radarPlot(rbind(rep(1,60), rep(0,60), medoids[48,-1]), 
           pfcol=colours[ref], seg = 5, vlcex = 1.6, 
           axistype=0, centerzero = TRUE, plwd = 1.5, 
           pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
mtext("e. CICADAS (Cluster 48)", side = 3, cex = 1.1, line = -0.1, adj=0)
ref <- ref + 1
radarPlot(rbind(rep(1,60), rep(0,60), medoids[49,-1]), 
           pfcol=colours[ref], seg = 5, vlcex = 1.6, 
           axistype=0, centerzero = TRUE, plwd = 1.5, 
           pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
mtext("f. PLANES (Cluster 49)", side = 3, cex = 1.1, line = -0.1, adj=0)
ref <- ref + 1
dev.off()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Radar plots alternative------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# initialise a dataframe for a 100 minute sample of each cluster
temp_indices_norm_summary <- data.frame(matrix(0, nrow = 100, ncol = 12))

ref <- 1
clust <- 34
if(clust %in% c(34,44, 48)) {
  col <- cicadas_col
}
if(clust %in% c(37, 3, 39)) {
  col <- birds_col
}
if(clust %in% c(29, 1)) {
  col <- insects_col
}
if(clust %in% c(59, 18)) {
  col <- rain_col
}
if(clust %in% c(42, 47)) {
  col <- wind_col
}
if(clust %in% c(41)) {
  col <- quiet_col
}
if(clust %in% c(23, 49)) {
  col <- planes_col
}

a <- which(indices_norm_summary$cluster_list==clust)

length(a)
a <- sample(a, 600)
length(a)
temp_indices_norm_summary <- indices_norm_summary[a,]
colnames(temp_indices_norm_summary) <- c("clust", "BGN","SNR","ACT",
                                         "EVN", "HFC", "MFC", "LFC",
                                         "ACI", "EAS", "EPS", "ECV",
                                         "CLC")

dev.off()
tiff(paste("C:/plos-visualization-paper/plots/radar_plot_alternative_clust", clust, ".tiff", sep=""), 
     height=900, width=900, res=300)
for(i in 1:nrow(temp_indices_norm_summary)){
  radarPlot(rbind(rep(1,60), rep(0,60), 
                  temp_indices_norm_summary[i,2:13]), 
            pfcol=col, seg = 3, vlcex = 0.8, 
            pcol=col, calcex =0.6, palcex = 0.6, pty=32,
            axistype=0, centerzero = TRUE, plwd = 0.6, 
            pdensity = 0, x1 = 1, y1 = 0, x2 = 1, y2 = 2,
            vlabels = rep("", 12))
  par(new=T)
}
radarPlot(rbind(rep(1,60), rep(0,60), medoids[clust,2:13]), 
          pfcol=col, seg = 3, vlcex = 0.8, caxislabels = NULL,
          pcol="black", calcex =0.6, palcex = 0.6, pty=32,
          axistype=0, centerzero = TRUE, plwd=1.2, 
          pdensity = 0, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
dev.off()

end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime

clust <- 39
tiff(paste("C:/plos-visualization-paper/plots/radar_plot_alternative_clust", clust, ".tiff", sep=""), 
     height=700, width=700, units = "px", res=300)
par(mfrow=c(1,1), xpd=NA, #decrease default margin
    mgp = c(0, 0.2, 0), cex = 0.6, oma = c(0, 0, 0, 0)) 
radarPlot(rbind(rep(1,60), rep(0,60), medoids[clust,-1]), 
          pfcol=colours[2], seg = 5, vlcex = 1.4, 
          axistype=0, centerzero = TRUE, plwd = 1.5, 
          pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
dev.off()
clust <- 41
tiff(paste("C:/plos-visualization-paper/plots/radar_plot_alternative_clust", clust, ".tiff", sep=""), 
     height=700, width=700, units = "px", res=300)
par(mfrow=c(1,1), xpd=NA, #decrease default margin
    mgp = c(0, 0.2, 0), cex = 0.6, oma = c(0, 0, 0, 0)) 
radarPlot(rbind(rep(1,60), rep(0,60), medoids[clust,-1]), 
          pfcol=colours[1], seg = 5, vlcex = 1.4, 
          axistype=0, centerzero = TRUE, plwd = 1.5, 
          pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
dev.off()
clust <- 13
tiff(paste("C:/plos-visualization-paper/plots/radar_plot_alternative_clust", clust, ".tiff", sep=""), 
     height=700, width=700, units = "px", res=300)
par(mfrow=c(1,1), xpd=NA, #decrease default margin
    mgp = c(0, 0.2, 0), cex = 0.6, oma = c(0, 0, 0, 0)) 
radarPlot(rbind(rep(1,60), rep(0,60), medoids[clust,-1]), 
          pfcol=colours[1], seg = 5, vlcex = 1.4, 
          axistype=0, centerzero = TRUE, plwd = 1.5, 
          pdensity = 60, x1 = 1, y1 = 0, x2 = 1, y2 = 2)
dev.off()
