getwd()
setwd("/Users/russellgreene/BCB546_Final_Project/Data/") #change this
source('final_functions.R')

#read in 2010 data

all.data <- read.table (file = "130319_Data.final.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

samp.data <- read.table(file = "110429_sampledata.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
samp.data <- aggregate(samp.data$Row, by = list(Plot = samp.data$Plot,Range = samp.data$Range, Block = samp.data$Rep., Genotype = samp.data$Variety),mean)
colnames(samp.data)[5] <- 'Row'
rerun.data <- merge(all.data,samp.data,by.x = 'sample',by.y = 'Plot')

el.names <- colnames(rerun.data)[12:32]
#rearrange the columns
rerun.data <- rerun.data[,c(1,12:32,34,33,36,35)] 
#this function is in the final_functions.R file
or.2010 <- outlierRemoveDataset(rerun.data,15,"sample",el.names)

#read in 2011 data
data.2011 <- read.table(file = "111101_Data.final.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
#sample list
samplelist.2011 <- read.table(file = "NAM Grain Sample List 2011.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

merge.2011 <- merge(data.2011, samplelist.2011, by.x = 'sample', by.y = 'Plot')
red.merge.2011 <- merge.2011[,c(1,12:33,35,34,36)] #keep unoutlier removed

#this function is in the final_functions.R file
or.2011 <- outlierRemoveDataset(merge.2011,15,"sample",el.names)
red.or.2011 <- or.2011[,c(1,12:36)]
red.or.2011$Year <- 2011
or.2010$Year <- 2010
red.merge.2011$Year <- 2011
rerun.data$Year <- 2010
noor.full.data <- rbind(rerun.data,red.merge.2011)  #this is the dataset with no outliers removed

#combine the years
full.data <- rbind(red.or.2011,or.2010)
#take average of all the seeds for a given plot
plot.ag <- aggregate(full.data[,el.names], by = list(Plot = full.data$sample,Genotype = full.data$Genotype, Row = full.data$Row, Range = full.data$Range, Block = full.data$Block, Year = full.data$Year),mean, na.rm = T)
