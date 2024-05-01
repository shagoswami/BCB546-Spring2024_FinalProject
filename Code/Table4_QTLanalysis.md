### Table 4 is output from a QTL analysis using genotypic data and the phenotypes from the elements to find associations between element concentrations and QTL. I am performing a QTL analysis for chromosome 1 using the data from the paper

setwd("C:/Users/kiarak/Desktop/Final")

#### READ IN the data files and label genotypes as parental lines - QTL needs segregating pop. I don't care about the other values and will make them blank

sdata <- read.table(file = "cornB73_IL14H.csv" , sep = ",", header = TRUE,stringsAsFactors = FALSE, na.strings = NA)   
start.col <- 7
lag.col <- start.col -1
p1 <- 'B73'
p2 <- 'Il14H'
blank.med <- apply(sdata[sdata$Line == 'blank',start.col:ncol(sdata)],2,median)
blank.sd <- apply(sdata[sdata$Line == 'blank',start.col:ncol(sdata)],2,sd)
for(i in start.col:ncol(sdata)){
  sdata[,i] <- sdata[,i] - blank.med[i-lag.col]
}


#### remove negative values and replace with zero values
sdata <- sdata[sdata$Line != 'blank',]
zerep <- function(x){  
  if(is.na(x) ==TRUE){return(x)}
  if(x < 0){
    return(0)
  }
  else{return(x)}
}

sdata[,start.col:ncol(sdata)] <- apply(sdata[,start.col:ncol(sdata)], c(1,2), zerep)
e.names <- names(sdata) #element and line values
el.names <- e.names[start.col:ncol(sdata)] #element columns only


#### outlier processing -- adapted function in final_functions to fit this data. using IQR instead of MADS for qtl analysis
x <- rep(1, nrow(sdata))
iqrout <- 3
numel <- ncol(sdata)
sum(is.na(sdata))
invisible(lapply(split(seq(nrow(sdata)), x), function(.ind){
  quartiles <-apply(sdata[.ind,start.col:numel],2,quantile, probs =c(0.1,0.9), na.rm = TRUE) 
  test <<-quartiles
  iqrs <- iqrout*(quartiles[2,] - quartiles[1,])   
  test2 <<-iqrs
  for (i in start.col:numel) { 
    .outl <- sdata[.ind, i] < (quartiles[1, i-lag.col] - iqrs[i-lag.col]) |
      sdata[.ind, i] > (quartiles[2, i-lag.col] + iqrs[i-lag.col])
    sdata[.ind, i][.outl] <<- NA
  }
}
))
sum(is.na(sdata))



### now to generate a map for QTL analysis


load("Corn_genotypes.RData") #R Datafile sent by Baxter with snp info

map.data <- sdata[sdata$Line != p1 & sdata$Line != p2,c(1,start.col:ncol(sdata))]



pheno.data <- aggregate(map.data[,2:ncol(map.data)],list(map.data$row),mean)
colnames(pheno.data)[1] <- 'row'

#### remove Al, Si, As, Se -- as stated in paper -- isobaric interferences (?) so they removed these elements
pheno.data <- pheno.data[,c(-5,-6,-17,-18)]

#### i'm only looking at chr 1 for the final and don't want all of the information so creating a file with reduced genotypic information
mark.data <- read.table(file = "Z011_reducedgenos.csv", sep =  ",",header = TRUE)

linecon <- rowinfo$line.names[rowinfo$ENTITYID %in% pheno.data$row]

#### get a common set of markers and lines
rmark.data <- mark.data[rownames(mark.data) %in% linecon,]

#### used help from Slack Exchange
linecon2 <- rowinfo$ENTITYID[rowinfo$line.names %in% rownames(rmark.data)]
rgenodata <- pheno.data[pheno.data$row %in% linecon2,]
rgenodata <- rgenodata[order(rgenodata$row),]
entlinelink <- rowinfo[rowinfo$ENTITYID %in% rgenodata$row,]
entlinelink <- entlinelink[order(entlinelink$ENTITYID),]
rgenodata <- rgenodata[order(entlinelink$line.names),]
entlinelink2 <- rowinfo[rowinfo$ENTITYID %in% rgenodata$row,]
cbind(entlinelink2[,c(1,7)], rgenodata$row)

t.data <- t(rmark.data)

repNA <- function(x) {
  if(is.na(x)){
    return("-")
  }else{return(x)}
}

t.data <- apply(t.data,c(1,2),repNA)

alljoin <- apply(t.data,1,function(x) paste(x, collapse = ""))
njoin <- names(alljoin)
noin <-  paste("*",njoin,sep = "")

names(alljoin) <- c(noin)

write.table(alljoin, file = "IL14H_finalmap.csv",sep = ",",quote = FALSE,col.names = FALSE)
colnames(rgenodata) <- paste("*",colnames(rgenodata),sep="")

write.table(t(rgenodata[,2:ncol(rgenodata)]), file = "IL14H_finalpheno.csv",sep = "\t",col.names = FALSE,quote = FALSE)

## These two files need to be combined for qtl analysis 
# I did this manually - using marker positions from https://repository.lib.ncsu.edu/server/api/core/bitstreams/5e042796-e939-4fff-80a5-2bd87f41b174/content for Chr. 1

# qtl analysis

## load libraries
library(lme4)
library(qtl)
library(qtlDesign)

## Load genotype data
geno <- read.cross("csv", file = "test3.csv", na.strings=c('NA'), genotypes = c("-1", "0", "1", "2"))
geno <- jittermap(geno) #because some markers are overlapping in positions
summary(geno)
#plot(geno)
#View(geno)
geno <- calc.genoprob(geno, step=0, off.end=0.0, error.prob=1.0e-4,stepwidth = "fixed", map.function="kosambi")

geno_cross <- sim.geno(geno,  n.draws=32, step=0, off.end=0.0, error.prob=1.0e-4, stepwidth = "fixed", map.function="kosambi")

scan.cim = cim(geno_cross, pheno.col=5, map.function="kosambi")

scan.cim.perm = cim(geno_cross, pheno.col=3, map.function="kosambi", n.perm=1000)

summary(scan.cim.perm)
summary(scan.cim, threshold = 0.5) #alpha = 0.05, can set alpha to other values 

### QTL scan visualization
plot(scan.cim)   
title(main="LOD scores for Chr1")


### allele density plots - can change this based on element and marker
#Plotting the QTL effect using the information from this Script
plotPXG(geno_cross, pheno.col = 6, marker = c("PZA03457.1")) # I changed the column for Selenium and the marker based on output from previous lines of code
title(main="QTL effect on selenium concentration - marker from my analysis")

#Plotting the QTL effect using the marker information given in the paper
plotPXG(geno_cross, pheno.col = 6, marker = c("PZA02698.3")) # I changed the column for Selenium and the marker based on output from previous lines of code
title(main="QTL effect on selenium concentration - marker from paper")


### I played with the values here based on reported QTL position and marker and the results from previous lines #####
#### These are results using the map position in their paper - p = sig
qtl <- makeqtl(geno_cross, chr=c(1), pos=c(161.7),what=c("prob")) #167.8 = position of marker in my file, 161.2 = their position
fitqtl <- fitqtl(geno, pheno.col=c(6), qtl= qtl, method = "hk")
summary(fitqtl)

#### These are results using the map position from this script = p = sig
qtl <- makeqtl(geno_cross, chr=c(1), pos=c(162),what=c("prob")) #167.8 = position of marker in my file, 161.2 = their position
fitqtl <- fitqtl(geno, pheno.col=c(6), qtl= qtl, method = "hk")
summary(fitqtl)

#### These are the results using the map position of the marker from the thesis - 167.8 - p= NS
qtl <- makeqtl(geno_cross, chr=c(1), pos=c(167.8),what=c("prob")) #167.8 = position of marker in my file, 161.2 = their position
fitqtl <- fitqtl(geno, pheno.col=c(6), qtl= qtl, method = "hk")
summary(fitqtl)


#### for error that figure margins are too large
par(mar=c(0.5,0.5,0.5,0.5))

lodint(results = scan.cim, chr = 1, drop = 1.8)


