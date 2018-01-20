# Title: Normalisation and Correlation Matrix - Plos One paper
# Author: Yvonne Phillips
# Date: 3 July 2015

# This code reads the summary indices and normalises and scales them.
# Then a correlation matrix is calculated and this is saved in a results folder

# File and folder requirements (2 files and 1 folder): 
# C:/plos-visualization-paper/data/Gympie_20150622_20160723_Towsey_Indices.csv
# C:/plos-visualization-paper/data/Woondum_20150622_20160723_Towsey_Indices.csv
# C:/plos-visualization-paper/results

# Time requirements: less than 1 mintue

##############################################
# Read Summary Indices
##############################################
# remove all objects in global environment
rm(list = ls())

gympie_file <- "C:/plos-visualization-paper/data/Gympie_20150622_20160723_Towsey_Indices.csv"
woondum_file <-"C:/plos-visualization-paper/data/Woondum_20150622_20160723_Towsey_Indices.csv"
gympie_indices <- read.csv(gympie_file, header = T)
woondum_indices <- read.csv(woondum_file, header = T)
indices_all <- rbind(gympie_indices, woondum_indices)

rm(gympie_file, woondum_file, gympie_indices, woondum_indices)

##############################################
# Normalise the selected summary indices
#############################################
# The code that follows shows how the normalised summary indices file
# were saved
# remove highly correlated indices
remove <- c(9,14)
indices_all <- indices_all[,-remove]
rm(remove)

# IMPORTANT:  These are used to name the plots
site <- c("Gympie NP", "Woondum NP")
index <- "SELECTED_Final" # or "ALL"
type <- "Summary"
paste("The dataset contains the following indices:"); colnames(indices_all)

# Generate a list of the missing minutes in summary indices
#missing_minutes_summary <- which(is.na(indices_all[,1]))
#save(missing_minutes_summary, file = "data/datasets/missing_minutes_summary_indices.RData")
# load missing_minutes_summary
#load(file="data/datasets/missing_minutes_summary_indices.RData")
missing_minutes_summary <- which(is.na(indices_all[,1]))
save(missing_minutes_summary, file="C:/plos-visualization-paper/data/missing_minutes_summary.RData")

length(missing_minutes_summary)
# There were 3 days where both microphones were not functioning correctly
# following rain.  These days were the 28, 29 and 30 October 2015
microphone_minutes <- c(184321:188640)
remove_minutes <- c(missing_minutes_summary, microphone_minutes)
z <- setdiff(1:nrow(indices_all), remove_minutes)

# remove NA values
indices_all <- indices_all[-c(remove_minutes),]

######### Normalise data #################################
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
#paste(indices_norm[417000,4], digits=15)
save(indices_norm, file="C:/plos-visualization-paper/results/normalised_indices.RData")
load(file="C:/plos-visualization-paper/results/normalised_indices.RData")
#paste(indices_norm[417000,4], digits=15)

##############################################
# Correlation matrix (Summary Indices) of thirteen months (398 days) at two sites
##############################################
cor <- abs(cor(indices_norm, use = "complete.obs"))

##############################################
# Save the correlation matrix
##############################################
write.csv(cor, file = "C:\\plos-visualization-paper\\results\\Correlation_matrix_norm.csv")
rm(cor)
# replace the missing minutes
complete_DF <- matrix(NA, nrow = (398*1440*2), ncol = 12)
complete_DF <- as.data.frame(complete_DF)
complete_DF[z,] <- indices_norm
colnames <- colnames(indices_norm)
colnames(complete_DF) <- colnames

save(complete_DF, file="C:\\plos-visualization-paper\\results\\Gympie_woondum_normalised_summary_indices.RData", 
          row.names = F)

rm(complete_DF, indices_norm)
