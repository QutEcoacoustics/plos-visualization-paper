# Title: PCA Diel Plot using Summary Indices - Plos one paper
# Author: Yvonne Phillips
# Date:  14 July 2016

# Note: The images produced are under copyright to the paper provided below 
# and must not be reproduced without attribution.
# These images are based on the summary acoustic indices calculated
# on one channel.

# Phillips, Y. F., Towsey, M., & Roe, P. (2018). Revealing the Ecological 
# Content of Long-duration Audio-recordings of the Environment through 
# Clustering and Visualisation. Plos One. 

# Description:  This code saves four PCA plots containing the mapping 
# of the scaled first three principal components to the red, green and 
# blue channel of the image. Plot size 4 x 10.7 MB.

# File and folder requirements (3 files and 1 folder): 
# These are all automatically loaded below
# C:/plos-visualization-paper/data/gympie2015062220160723towseyindices.csv
# C:/plos-visualization-paper/data/woondum2015062220160723towseyindices.csv
# C:/plos-visualization-paper/data/geoscienceaustraliasunrisetimesgympie20152016
# C:/plos-visualization-paper/plots

# Time requirements: about 3 minutes

# Packages: NILL
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PCA plot ------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# remove all objects in the global environment
rm(list = ls())
start_time <- paste(Sys.time())

# make folder for plots if it does not exist
f <- paste0("C:/plos-visualization-paper/plots/")
if (!dir.exists(f)) {
  dir.create("C:/plos-visualization-paper/plots/")
}

# Load and read summary indices (if necessary)
u <- "https://data.researchdatafinder.qut.edu.au/dataset/0f706895-37df-4a15-a7a6-3c9e5e2a3dd0/resource/c5b88663-5ed5-47b6-bddd-1069ec6b21d0/download/gympie2015062220160723towseyindices.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/",name,sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/", basename(u)))
  rm(f, u)
}
gympie_indices <- read.csv(paste0("C:/plos-visualization-paper/data/",name,sep=""))

u <- "https://data.researchdatafinder.qut.edu.au/dataset/0f706895-37df-4a15-a7a6-3c9e5e2a3dd0/resource/0869bf65-0951-4636-ae4a-fec37528a32d/download/woondum2015062220160723towseyindices.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/",name,sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/", basename(u)))
  rm(f, u)
}
if (file.exists(f)) {
  rm(f, u)
}
woondum_indices <- read.csv(paste0("C:/plos-visualization-paper/data/",name,sep=""))
indices_all <- rbind(gympie_indices, woondum_indices)
rm(gympie_indices, woondum_indices)

##############################################
# Normalise the selected summary indices
#############################################
# remove one of the pairs of highly correlated indices
remove <- c(9,14)
indices_all <- indices_all[,-remove]
rm(remove)

######### Normalise function #################################
normalise <- function (x, xmin, xmax) {
  y <- (x - xmin)/(xmax - xmin)
}
###########################################################
# Create a normalised dataset between 1.5 and 98.5% bounds 
###########################################################
indices_norm <- indices_all

# normalise values between 1.5 and 98.5 percentile bounds
q1.values <- NULL
q2.values <- NULL
for (i in 1:ncol(indices_all)) {
  q1 <- unname(quantile(indices_all[,i], probs = 0.015, na.rm = TRUE))
  q2 <- unname(quantile(indices_all[,i], probs = 0.985, na.rm = TRUE))
  q1.values <- c(q1.values, q1)
  q2.values <- c(q2.values, q2)
  indices_norm[,i]  <- normalise(indices_all[,i], q1, q2)
}
rm(q1, q2, i)

# adjust values greater than 1 or less than 0 to 1 and 0 respectively
for (j in 1:ncol(indices_norm)) {
  a <- which(indices_norm[,j] > 1)
  indices_norm[a,j] = 1
  a <- which(indices_norm[,j] < 0)
  indices_norm[a,j] = 0
}

indices_all <- indices_norm

# Set the first day of recording and the date 
# in the format (YYYY-MM-DD)
actual_start_date <- "2015-06-22" # the first day of the recording 
start_date <- "2015-09-01"        # the date where the plot will start
actual_end_date <- "2016-07-23"
end_date <- "2016-02-01"
interval <- 1440
start <- as.POSIXct(actual_start_date)
end <- as.POSIXct(actual_end_date)
dates <- seq(from=start, 
             by=interval*60, to=end)
total_days <- length(dates)
rm(start, end)
first_of_month <- which(substr(dates, 9, 10)=="01")
d <- which(substr(dates,1,10)==start_date)
e <- which(substr(dates,1,10)==end_date)

# convert dates to date-time format
start <- as.POSIXct(start_date)
end <- as.POSIXct(end_date)
#end <- start + as.difftime(days, units="days")
dates <- seq(from=start, by=interval*60, to=end)

# IMPORTANT:  These are used to name the plots
site <- c("Gympie NP", "Woondum NP")
type <- "Summary"

# Print out a list of all summary indices
paste("The dataset contains the following indices:"); colnames(indices_all)

# Generate a list of the missing minutes in summary indices
missing_minutes_summary <- which(is.na(indices_all[,1]))

# obtain a list of all recorded minutes
z <- setdiff(1:nrow(indices_all), missing_minutes_summary)

# List of summary indices columns:
#[1]  "BGN"  Background Noise;    [2]  "SNR"  Signal to Noise ratio
#[3]  "ACT"  Activity;            [4]  "EVN"  Events per second
#[5]  "HFC"  High Frequency Cover [6]  "MFC"  Mid Frequency Cover
#[7]  "EVN"  Low Frequency Cover  [8]  "ACI"  Acoustic Complexity Index
#[9]  "EAS"  Entropy of Average Spectrum
#[10] "EPS"  Entropy of Peak Spectrum
#[11] "ECV"  Entropy of Coefficient of Variance
#[12] "CLC"  Cluster count

length(indices_all[,1])

# remove the missing minutes
indices_norm <- indices_all[-c(missing_minutes_summary),]

# preform pca analysis
indices_pca <- prcomp(indices_norm, scale. = F)
indices_pca$PC1 <- indices_pca$x[,1]
indices_pca$PC2 <- indices_pca$x[,2]
indices_pca$PC3 <- indices_pca$x[,3]
indices_pca$PC4 <- indices_pca$x[,4]
indices_pca$PC5 <- indices_pca$x[,5]
indices_pca$PC6 <- indices_pca$x[,6]
indices_pca$PC7 <- indices_pca$x[,7]

pca_coef <- cbind(indices_pca$PC1, indices_pca$PC2,
                  indices_pca$PC3, indices_pca$PC4,
                  indices_pca$PC5, indices_pca$PC6,
                  indices_pca$PC7)
rm(indices_pca)

coef_min_max <- pca_coef[,1:3]

# Scale the PCA coefficients between 0 and 1 so they can be 
# mapped to red, green and blue channels.
coef_min_max_norm <- coef_min_max
min.values <- NULL
max.values <- NULL
for (i in 1:3) {
  min <- unname(quantile(pca_coef[,i], probs = 0.0, na.rm = TRUE))
  max <- unname(quantile(pca_coef[,i], probs = 1.0, na.rm = TRUE))
  min.values <- c(min.values, min)
  max.values <- c(max.values, max)
  coef_min_max_norm[,i]  <- normalise(coef_min_max[,i], min, max)
}

pca_coeff <- matrix(NA, nrow = nrow(indices_all), ncol = 3)
pca_coeff[z,] <- coef_min_max_norm
rm(coef_min_max_norm)
# check the list of missing rows
rows <- which(is.na(pca_coeff[,1]))

# rows where there are na should equal the missing_minutes_summary 
# vector, the function below should be zero
check <- setdiff(rows, missing_minutes_summary)
length(check) # should be 0 showing there is no difference

# replace the missing rows with the number one 
# this is so that these mintues are printed as white
pca_coeff[missing_minutes_summary,] <- rep(1, 3)

# select a subset in relation to the start and end date
coef_min_max <- pca_coeff[c(((d-1)*1440):(e*1440-1),
                               (((d+total_days)-1)*1440):((e+total_days)*1440-1)),]
rm(pca_coeff)
# generate a date sequence and locate the first of the month
days <- length(coef_min_max[,1])/(2*1440)

# Load (if requried) and read the civil dawn, civil dusk and sunrise and sunset times
# Based on Geoscience Australia material
# http://www.ga.gov.au/geodesy/astro/sunrise.jsp
# Please note these sunrise times are specific to location
u <- "https://data.researchdatafinder.qut.edu.au/dataset/ed90afd5-6793-4491-b2cc-6e2b4cf01dd9/resource/098982e4-980a-4d29-9652-fb93c2d89f27/download/geoscienceaustraliasunrisetimesgympie20152016.csv"
name <- basename(u)
f <- paste0("C:/plos-visualization-paper/data/",name,sep="")
if (!file.exists(f)) {
  download.file(u, file.path("C:/plos-visualization-paper/data/", basename(u)))
  rm(f, u)
}
if (file.exists(f)) {
  rm(f, u)
}

civil_dawn <- read.csv(paste0("C:/plos-visualization-paper/data/",name,sep=""))

# convert minutes to 24 hour time
civil_dawn$civil_dawn_times <- 
  paste(substr(civil_dawn$CivSunrise,1,1), ":",
        substr(civil_dawn$CivSunrise,2,3), sep="")
civil_dawn$civil_dusk_times <- 
  paste(substr(civil_dawn$CivSunset,1,2), ":",
        substr(civil_dawn$CivSunset,3,4), sep="")
civil_dawn$sunrise <- 
  paste(substr(civil_dawn$Sunrise,1,1), ":",
        substr(civil_dawn$Sunrise,2,3), sep="")
civil_dawn$sunset <- 
  paste(substr(civil_dawn$Sunset,1,2), ":",
        substr(civil_dawn$Sunset,3,4), sep="")
civil_dawn$dates <- as.character(civil_dawn$dates)
a <- which(nchar(civil_dawn$dates)==9)
civil_dawn$dates[a] <- paste("0",civil_dawn$dates[a], sep = "")
civil_dawn$dates <- paste(substr(civil_dawn$dates,7,10), "-",
                          substr(civil_dawn$dates,4,5), "-",
                          substr(civil_dawn$dates,1,2), sep = "")

a <- which(civil_dawn$dates==substr(start, 1, 10))
reference <- a:(a+days-1)
civil_dawn_times <- civil_dawn$civil_dawn_times[reference]
civil_dusk_times <- civil_dawn$civil_dusk_times[reference]
sunrise_times <- civil_dawn$sunrise[reference]
sunset_times <- civil_dawn$sunset[reference]

civ_dawn <- NULL
for(i in 1:length(civil_dawn_times)) {
  hour <- as.numeric(substr(civil_dawn_times[i], 1,1))
  min <- as.numeric(substr(civil_dawn_times[i], 3,4))
  minute <- hour*60 + min
  civ_dawn <- c(civ_dawn, minute)
}

civ_dusk <- NULL
for(i in 1:length(civil_dusk_times)) {
  hour <- as.numeric(substr(civil_dusk_times[i], 1,2))
  min <- as.numeric(substr(civil_dusk_times[i], 4,5))
  minute <- hour*60 + min
  civ_dusk <- c(civ_dusk, minute)
}

sunrise <- NULL
for(i in 1:length(sunrise_times)) {
  hour <- as.numeric(substr(sunrise_times[i], 1,1))
  min <- as.numeric(substr(sunrise_times[i], 3,4))
  minute <- hour*60 + min
  sunrise <- c(sunrise, minute)
}

sunset <- NULL
for(i in 1:length(sunset_times)) {
  hour <- as.numeric(substr(sunset_times[i], 1,2))
  min <- as.numeric(substr(sunset_times[i], 4,5))
  minute <- hour*60 + min
  sunset <- c(sunset, minute)
}

# plot the original pca plot
style <- "false_colour"

# produce pca diel plots for both sites ##
dev.off()
for (k in 1:2) {
  ref <- c(0, days*1440)
  # generate a date sequence and locate the first of the month
  days <- length(coef_min_max[,1])/(2*1440)
  #start <- as.POSIXct("2015-06-22")
  #start <- as.POSIXct(start)
  interval <- 1440
  end <- start + as.difftime(days, units="days")
  dates <- seq(from=start, by=interval*60, to=end)
  tiff(filename = paste("C:/plos-visualization-paper/plots/PCA_plot_",site[k],"_", 
                        type,"_",  start_date,"_", 
                        end_date, style,".tif", sep = ""),
       width = 2600, height = 1400, units = "px",
       res = 300)
  par(mar=c(1.1, 3.4, 1, 3.4))
  # Plot an empty plot with no axes or frame
  plot(c(0,1440), c(days,1), type = "n", axes=FALSE, 
       frame.plot=FALSE,
       xlab="", ylab="") #, asp = 398/1440)
  # Create the heading
  mtext(side=3, line = 0.3,
        paste(site[k]," ", format(dates[1], "%d %B %Y")," - ", 
              format(dates[length(dates)-1], "%d %B %Y"), sep=""), 
        cex=0.7)
  # Create the sub-heading
  mtext(side=3, line = -0.3, 
        paste(type, " indices PCA coefficients", sep = ""),
        cex=0.6)
  
  # draw coloured polygons row by row
  ref <- ref[k]
  # set the rows starting at the top of the plot
  for(j in days:1) {
    # set the column starting on the left
    for(l in 1:1440) {
      ref <- ref + 1
      # draw a square for each minute in each day 
      # using the polygon function mapping the red, green
      # and blue channels to the normalised pca-coefficients
      if(style=="false_colour") {
        polygon(c(l,l,l+1,l+1), c(j,(j-1),(j-1),j),
                col=rgb(coef_min_max[ref,1],
                        coef_min_max[ref,2],
                        coef_min_max[ref,3]),
                border = NA)  
      }
      if(style=="colour_blind") {
        polygon(c(l,l,l+1,l+1), c(j,(j-1),(j-1),j),
                col=coef_min_max[ref,4],
                border = NA)  
      }
    }
  }
  
  # draw horizontal lines
  first_of_month <- which(substr(dates, 9, 10)=="01")
  first_of_each_month <- days - first_of_month + 1
  for(i in 1:length(first_of_month)) {
    lines(c(1,1441), c(first_of_each_month[i], 
                       first_of_each_month[i]), 
          lwd=1, lty = 3)
  }
  # draw vertical lines
  at <- seq(0,1440, 120) + 1
  for(i in 1:length(at)) {
    lines(c(at[i], at[i]), c(1,days), lwd=1, lty=3)
  }
  # label the x axis
  axis(1, tick = FALSE, at = at, 
       labels = c("12 am","2","4",
                  "6","8","10", "12",
                  "2","4","6",
                  "8","10","12 pm"), 
       cex.axis=0.85, line = -1.6)
  mtext(side=1, line=0.16, "Time (24 hours)", cex=0.9)
  # plot the left axes
  axis(side = 2, at = first_of_each_month, tick = FALSE, 
       labels=format(dates[first_of_month],"%d %b %Y"), 
       cex.axis=0.9, las=1, line = -2.3, hadj=1.05)
  # plot the left axes
  axis(side = 4, at = first_of_each_month, tick = FALSE, 
       labels=format(dates[first_of_month],"%d %b %Y"), 
       cex.axis=0.9, las=1, line = -2.3, hadj=-0.01)
  at <- seq(0, 1440, 240)
  # draw dotted line to show civil-dawn
  for(i in length(civ_dawn):1) {
    lines(c(civ_dawn+1), c(length(civ_dawn):1),  
          lwd=1.2, lty=2, col="black")
  }
  # draw line to show civil-dusk
  for(i in length(civ_dawn):1) {
    lines(c(civ_dusk+1), c(length(civ_dusk):1),  
          lwd=1.2, lty=2, col="black")
  }
  # draw line to show sunrise
  for(i in length(sunrise):1) {
    lines(c(sunrise+1), c(length(sunrise):1),  
          lwd=1.2, lty=2, col="black")
  }
  # draw line to show sunset
  for(i in length(sunset):1) {
    lines(c(sunset+1), c(length(sunset):1),  
          lwd=1.2, lty=2, col="black")
  }
  dev.off()
}

# plot the colour-blind version of the pca plot
style <- "colour_blind"

# colours from ColourBrewer2 
# Brewer, C., Harrower, M., & The Pennsylvania State University.
# (2017). ColourBrewer 2.   Retrieved from http://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=11

colour_blind_colours2 <- c("#a50026","#ffffbf",
                           "#f46d43","#fdae61",
                           "#fee090","#d73027",
                           "#313695","#abd9e9",
                           "#74add1","#4575b4",
                           "#e0f3f8")
# To view the colours
#library(scales)
#show_col(colour_blind_colours2)

set.seed(12)
clusters <- kmeans(coef_min_max[,1:3], centers = 11, iter.max=50) 
coef_min_max <- data.frame(coef_min_max)
coef_min_max$col <- colour_blind_colours2[clusters$cluster]

# produce pca diel plots for both sites ##
dev.off()
for (k in 1:2) {
  ref <- c(0, days*1440)
  # generate a date sequence and locate the first of the month
  days <- length(coef_min_max[,1])/(2*1440)
  #start <- as.POSIXct("2015-06-22")
  #start <- as.POSIXct(start)
  interval <- 1440
  end <- start + as.difftime(days, units="days")
  dates <- seq(from=start, by=interval*60, to=end)
  tiff(filename = paste("C:/plos-visualization-paper/plots/PCA_plot_",site[k],"_", 
                        type,"_", start_date,"_", 
                        end_date, style,".tif", sep = ""),
       width = 2600, height = 1400, units = "px",
       res = 300)
  par(mar=c(1.1, 3.4, 1, 3.4))
  # Plot an empty plot with no axes or frame
  plot(c(0,1440), c(days,1), type = "n", axes=FALSE, 
       frame.plot=FALSE,
       xlab="", ylab="") #, asp = 398/1440)
  # Create the heading
  mtext(side=3, line = 0.3,
        paste(site[k]," ", format(dates[1], "%d %B %Y")," - ", 
              format(dates[length(dates)-1], "%d %B %Y"), sep=""), 
        cex=0.7)
  # Create the sub-heading
  mtext(side=3, line = -0.3, 
        paste(type, " indices PCA coefficients", sep = ""),
        cex=0.6)
  
  # draw coloured polygons row by row
  ref <- ref[k]
  # set the rows starting at the top of the plot
  for(j in days:1) {
    # set the column starting on the left
    for(l in 1:1440) {
      ref <- ref + 1
      # draw a square for each minute in each day 
      # using the polygon function mapping the red, green
      # and blue channels to the normalised pca-coefficients
      if(style=="false_colour") {
        polygon(c(l,l,l+1,l+1), c(j,(j-1),(j-1),j),
                col=rgb(coef_min_max[ref,1],
                        coef_min_max[ref,2],
                        coef_min_max[ref,3]),
                border = NA)  
      }
      if(style=="colour_blind") {
        polygon(c(l,l,l+1,l+1), c(j,(j-1),(j-1),j),
                col=coef_min_max[ref,4],
                border = NA)  
      }
    }
  }
  
  # draw horizontal lines
  first_of_month <- which(substr(dates, 9, 10)=="01")
  first_of_each_month <- days - first_of_month + 1
  for(i in 1:length(first_of_month)) {
    lines(c(1,1441), c(first_of_each_month[i], 
                       first_of_each_month[i]), 
          lwd=1, lty = 3)
  }
  # draw vertical lines
  at <- seq(0,1440, 120) + 1
  for(i in 1:length(at)) {
    lines(c(at[i], at[i]), c(1,days), lwd=1, lty=3)
  }
  # label the x axis
  axis(1, tick = FALSE, at = at, 
       labels = c("12 am","2","4",
                  "6","8","10", "12",
                  "2","4","6",
                  "8","10","12 pm"), 
       cex.axis=0.85, line = -1.6)
  mtext(side=1, line=0.16, "Time (24 hours)", cex=0.9)
  # plot the left axes
  axis(side = 2, at = first_of_each_month, tick = FALSE, 
       labels=format(dates[first_of_month],"%d %b %Y"), 
       cex.axis=0.9, las=1, line = -2.3, hadj=1.05)
  # plot the left axes
  axis(side = 4, at = first_of_each_month, tick = FALSE, 
       labels=format(dates[first_of_month],"%d %b %Y"), 
       cex.axis=0.9, las=1, line = -2.3, hadj=-0.01)
  
  at <- seq(0, 1440, 240)
  # draw dotted line to show civil-dawn
  for(i in length(civ_dawn):1) {
    lines(c(civ_dawn+1), c(length(civ_dawn):1),  
          lwd=1.2, lty=2, col="black")
  }
  # draw line to show civil-dusk
  for(i in length(civ_dawn):1) {
    lines(c(civ_dusk+1), c(length(civ_dusk):1),  
          lwd=1.2, lty=2, col="black")
  }
  # draw line to show sunrise
  for(i in length(sunrise):1) {
    lines(c(sunrise+1), c(length(sunrise):1),  
          lwd=1.2, lty=2, col="black")
  }
  # draw line to show sunset
  for(i in length(sunset):1) {
    lines(c(sunset+1), c(length(sunset):1),  
          lwd=1.2, lty=2, col="black")
  }
  dev.off()
}
end_time <- paste(Sys.time())
diffDateTime <- as.POSIXct(end_time) - as.POSIXct(start_time)
diffDateTime