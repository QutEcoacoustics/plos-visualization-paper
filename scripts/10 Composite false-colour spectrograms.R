# Title:  Composite false-colour spectrograms - Big Data Visual Analytics paper
# Author: Yvonne Phillips
# Date:  20 January 2017

# Phillips, Y., Towsey, M., & Roe, P. (2017, 6-10 November). Visualization
# of environmental audio using ribbon plots and acoustic state sequences. 
# Paper presented at the IEEE International Symposium on Big Data Visual 
# Analytics (BDVA), Adelaide, Australia.

# Description:  This code generates a composite false colour
# spectrogram for each cluster. A composite false colour spectrogram
# is an image containing the false colour representation of 600 randomly
# sampled minutes from each cluster. This code needs access to a folder
# containing the 24-hour false colour spectrograms. For the purpose of 
# demonstrating this code, two 24-four hour false colour spectrograms
# have been provided.
# NOTE: This code will not work without downloading the 24-hour false-colour
# spectrograms in the specified "path" eg. C:/plos-visualization-paper/data/spectrograms/site1

# Files (cluster list and the 24 hour false-colour spectrograms) and folder requirements
# C:/plos-visualization-paper/data/gympieclusterlist
# C:/plos-visualization-paper/data/woondumclusterlist
# C:/plos-visualization-paper/data/spectrograms/GympieNP/
# C:/plos-visualization-paper/data/spectrograms/WoondumNP/
# Note: the 24-hour false-colour spectrograms will be downloaded from
# https://data.researchdatafinder.qut.edu.au/dataset/c3e4340d-4f56-45bf-85a6-2adb5dbf3d6b/resource/968b1492-619b-47ae-ab7d-fad88a9fa6a7/download/gympienp.zip
# https://data.researchdatafinder.qut.edu.au/dataset/c3e4340d-4f56-45bf-85a6-2adb5dbf3d6b/resource/313a2107-8963-40bf-9934-fe9cbe458260/download/woondumnp.zip
# The two zip files are 500 MB each, total 1 GB

# Time required to run code and produce 4 composite spectrograms: 
# 6.5 minutes (about 1.5 mintues for each composite spectrogram)

# Package requirements: raster

#########################################################
# Composite false-colour spectrograms -----------------------------
#########################################################
# remove all objects in the global environment
rm(list = ls())
start_time <- paste(Sys.time())

# create directory for images if it does not exist
f <- paste0("C:/plos-visualization-paper/data/spectrograms/GympieNP/")
if (!dir.exists(f)) {
  dir.create("C:/plos-visualization-paper/data/spectrograms/GympieNP/")
}

f <- paste0("C:/plos-visualization-paper/data/spectrograms/WoondumNP/")
if (!dir.exists(f)) {
  dir.create("C:/plos-visualization-paper/data/spectrograms/WoondumNP/")
}
# Note the next lines will download 1 GB of data
# Download the Gympie spectrograms
u <- "https://data.researchdatafinder.qut.edu.au/dataset/c3e4340d-4f56-45bf-85a6-2adb5dbf3d6b/resource/968b1492-619b-47ae-ab7d-fad88a9fa6a7/download/gympienp.zip"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/spectrograms/GympieNP/", name, sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/spectrograms/GympieNP/", basename(u)))
  rm(f, u)
}
unzip(zipfile = "C:/plos-visualization-paper/data/spectrograms/GympieNP/gympienp.zip",
      files = "C:/plos-visualization-paper/data/spectrograms/GympieNP/")
# Download the Woondum spectrograms
u <- "https://data.researchdatafinder.qut.edu.au/dataset/c3e4340d-4f56-45bf-85a6-2adb5dbf3d6b/resource/313a2107-8963-40bf-9934-fe9cbe458260/download/woondumnp.zip"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/spectrograms/WoondumNP/", name, sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/spectrograms/WoondumNP/", basename(u)))
  rm(f, u)
}
unzip(zipfile = "C:/plos-visualization-paper/data/spectrograms/WoondumNP/woondumnp.zip",
      files = "C:/plos-visualization-paper/data/spectrograms/WoondumNP/")

# Load (if necessary) and read the cluster list
# Load the Gympie cluster list
u <- "https://data.researchdatafinder.qut.edu.au/dataset/62de1856-d030-423b-9ada-0b16eb06c0ba/resource/7a70163b-323b-4c30-aaf3-e19e934b328d/download/gympieclusterlist.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/",name,sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/", basename(u)))
  rm(f, u)
}
gympie_cluster_list <- read.csv(paste0("C:/plos-visualization-paper/data/",name,sep=""))

# Load the Woondum cluster list
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

# write cluster list csv files for each site
write.csv(cluster_list[1:(length(cluster_list)/2)], "C:/plos-visualization-paper/data/gympie_cluster_list.csv", row.names = F)
write.csv(cluster_list[(length(cluster_list)/2+1):(length(cluster_list))], "C:/plos-visualization-paper/data/woondum_cluster_list.csv", row.names = F)

k1_value <- 25000
k2_value <- 60

days <- length(cluster_list)/(1440)
minute_reference <- rep(0:1439, days)

cluster_list <- cbind(cluster_list, minute_reference)
rm(days, minute_reference)

cluster_list <- data.frame(cluster_list)

# find the first minute of each day
first_minutes <- which(cluster_list$minute_reference=="0")
# find the last minute of each day
last_minutes <- which(cluster_list$minute_reference=="1439")

first_minutes <- cbind(first_minutes, last_minutes)
colnames(first_minutes) <- c("start","end")

first_minutes <- as.data.frame(first_minutes)

# Install the raster package if it has not been previously installed
packages <- c("raster")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(raster)

# plot and save an image containing a black rectangle into
# a file named 'Rasterimage.pgn' this will be used to produce
# the composite spectrograms
tiff('C:/plos-visualization-paper/data/Rasterimage.tiff', 
     width=615, height=668, 
     units='px', type='cairo', antialias=NULL)
z <- matrix(0, ncol=615, nrow=668)
par(mar=c(0,0,0,0))
plot.new()
rasterImage(z,-0.04,-0.04,1.04,1.04,interpolate=FALSE)
dev.off()

image <- "C:/plos-visualization-paper/data/Rasterimage.png"

s <- brick(image, package="raster", ncol=615, nrows=668)
#s <- brick(image, package="raster", ncol=1280, nrows=668)
s[[1]] <- 255 
s[[2]] <- 255
s[[3]] <- 255
s <- subset(s,1:3)

# Set the path to the folder containing all of the 24-hour spectrograms
# this folder can be two or more levels up from the actual files because the 
# spectrogram files will be found recursively (see next arguement)
path <- "C:/plos-visualization-paper/data/spectrograms" # path to spectrogram files

# generate a list of all files containing the 24-hour spectrograms from
# Gympie eg. "C:/plos-visualization-paper/data/spectrograms/GympieNP/20150622/GympieNP_20150622__2Maps.png"
spect_file_Gympie <- list.files(full.names=TRUE, recursive = T,
                                path = paste(path,"\\GympieNP",sep=""),
                                pattern = "*2Maps.png")
# generate a list of all files containing the 24-hour spectrograms from
# Woondum eg. "C:/plos-visualization-paper/data/spectrograms/WoondumNP/20150622/WoondumNP_20150622__2Maps.png"
spect_file_Woondum <- list.files(full.names=TRUE, recursive = T,
                                 path = paste(path,"\\WoondumNP",sep=""),
                                 pattern = "*2Maps.png") 
spect_file_list <- c(spect_file_Gympie, spect_file_Woondum)
# check the length of this list is the same as the nrow in first_minutes
length(spect_file_list)
nrow(first_minutes)

# this function chooses the random sample, plots and saves the
# composite false-colour spectrogram for a selection of cluster
# numbers. 
# NOTE: This is designed for the 24-hour false colour spectrograms
# generated by Towsey (2014) where the size of the 24-hour spectrograms
# are 1440 (width) x 668 (height) pixels

cluster_image <- function(clust_num) {
  sample_size <- 600
  # Get list of positions of cluster
  which1 <- which(cluster_list$cluster_list==clust_num)
  # Select a random sample from a cluster
  whichV1 <- sample(which1, sample_size)
  # Create a blank raster image
  # Use ncol =615 for 600 minutes with RasterImage1.png
  s <- brick(image, package="raster", ncol=615, nrows=668)
  # Use ncol = 1280 for 1200 minutes with RasterImage.png
  #s <- brick(image, package="raster", ncol=1280, nrows=668)
  s[[1]] <- 255 
  s[[2]] <- 255
  s[[3]] <- 255
  s <- subset(s,1:3)
  tiff(paste("C:/plos-visualization-paper/plots/ClusterImage_Cluster",
             clust_num,"_", k1_value, "_", k2_value, 
             ".tiff", sep = ""), width = 2250, 
       height = 2350, units = 'px', res = 300)
  # Set the start column from the edge of the image
  length2 <- 10
  min.ref.check <- NULL
  which.check <- NULL
  
  for(i in 1:sample_size) {
    if(whichV1[i] %in% c(seq(1440,length(spect_file_list)*1440, 1440))){
      day.ref <- floor((whichV1[i])/1440)
    }
    else {
      day.ref <- floor((whichV1[i])/1440)+1
    }
    min.ref <- ((whichV1[i]/1440) - (day.ref-1))*1440
    # select the twenty-four hour spectrogram image
    b1 <- spect_file_list[day.ref]
    # read spectrogram image as a raster image (sourceImage)
    b <- brick(b1, package="raster")
    sourceImage <- brick(b1, package="raster")
    current.minute.list <- min.ref
    replacementBlock <- getValuesBlock(sourceImage, 
                                       row=1, 
                                       nrows=668, 
                                       col=current.minute.list, 
                                       ncols=1)
    s[1:668, length2] <- replacementBlock
    length2 <- length2 + 1
    which.check <- c(which.check, whichV1[i])
    min.ref.check <- c(min.ref.check, min.ref)
  }
  plotRGB(s)
  dev.off()
}

# Call the cluster_image function for each of the clusters
dev.off()
clusters <- c(59,42,29,37)
for(j in clusters) {
  print(paste("starting", j, Sys.time(), sep = " "))
  cluster_image(j)
}
end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime
