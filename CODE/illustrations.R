#------------------ SETUP ----------------------------------------------

setwd('~/Dropbox/viewsForecastingChallenge/CLEAN/')
rm(list=ls())
gc()
library(arrow) # useful to import parquet files from Python
library(TSclust)
library(zoo)
library(readr)

# https://views.pcr.uu.se/download/datasets/

# Load functions
source('Functions/tadda.R')  
source('load.data.R')


#------------------ IMPORT DATA ----------------------------------------------

# Important: Choose pgm or cm below
this.unit <- 'cm'
#this.unit <- 'pgm'

# Define the STEP to train, predict and evaluate for. 
step <- 7
df1 <- load.data(step = step, 
                 this.unit = this.unit)
--------
  
  #An example:
  pdf('../WriteUp/Figs/nigeria380.pdf')
par(bty='o', pty='s', mfrow=c(1,1), mar=c(4,6,1,1))
plot(c(369:380),
     c(0.5108256,  0.3364722, -1.9459101,  1.9459101, 0.6190392, -0.1670541, -0.7884574, -1.6094379,3.4657359, -0.5753641,  0.5753641, -0.1698990),
     type='b', xlab='Month ID',
     ylab=expression(Delta ~ 'ln(casualties)'),#'dln_ged_best_sb',
     cex.lab=2.5, cex.axis=2, las=1, 
     main = '', lwd=2, cex=1.5, pch=19)
yline(0, lty=2, col='grey', lwd=3)
dev.off()  

# Mozambique: 162
# Egypt: 222
# Cameroon: 69
#PGID: 149426
cmids <- c(162, 222,69)
pgids <- c(149426,149427,149428,149429,149430, 148706,148707,148708,148709,148710, 147986, 147987,147988,147989,147990, 147266,147267,147268,147269,147270, 146546,146547,146548, 146549,146550)
pgidsNig <- unique(df1$pg_id[df1$country_name=='Nigeria'])
pgids <- pgids[pgids%in%pgidsNig]

listOfAllUnits <- unique(df1$unit)
for(thissourceID in pgids){
  if(df1$country_name[df1$pg_id==thissourceID][1] == 'Nigeria'){
    source <- df1$output[  df1$month_id %in% c((487-11):487) &
                             df1$unit==thissourceID]
    
    for(i in 1:10000){
      if(i%%1000==0) print(i)
      targetMonth_id <- sample(1:(487-11), size=1)
      targetUnit <- sample(listOfAllUnits, size=1)
      target <- df1$output[df1$month_id%in% c((targetMonth_id-11):targetMonth_id) & 
                             df1$unit == targetUnit ]
      if(length(which(is.na(target))) ==0){
        if(length(target)!=0){
          this.diss <- as.numeric(diss(rbind(source, target), METHOD = 'DTWARP'))
          res <- data.frame(sourceID = thissourceID, sourceMonth_id = 487, 
                            targetID = targetUnit, targetMonth_id = targetMonth_id,
                            distance = this.diss)
          write.table(x = res, paste(this.unit, 'examples.csv', sep='_'),
                      sep = ',',  append = T,
                      row.names = F)
        }
      }
    }
  }
}

ex <- read.csv(paste(this.unit, 'examples.csv', sep='_'))
ex$distance <- as.numeric(as.character(ex$distance))
ex <- ex[order(ex$distance), ]
ex <-ex[-(which(duplicated(ex))),]
ex1 <- ex

pred <- read.csv('Predictions/Views/ViewsForecasting_chadefaux/chadefaux_cm_set3.csv')
me1 <- merge(df1, pred, by.y=c('month_id', 'unit_id'),
             by.x = c('month_id', 'country_id'), all.x=T)

# Mozambique: 162
# Egypt: 222
# Cameroon: 69
pdf(paste('../WriteUp/Figs/SamplePredictions_',this.unit,'.pdf', sep=''))
#par(mfrow=c(7,4), mar=c(0,0,0,0), pty='s')
par(mfrow=c(3,4), mar=c(0,1,0,0), pty='s')
for(thissourceID in cmids){ #pgids
  #if(df1$country_name[df1$pg_id==thissourceID][1] == 'Nigeria'){
  ex <- ex1[ex1$sourceID==thissourceID,]
  source <- df1$output[  df1$month_id %in% c((487-11):487) &
                           df1$unit==thissourceID]
  source.pred2 <- me1$chadefaux_s2[me1$month_id ==490 &
                                     me1$unit==thissourceID]
  source.pred3 <- me1$chadefaux_s3[me1$month_id ==491 &
                                     me1$unit==thissourceID]
  source.pred4 <- me1$chadefaux_s4[me1$month_id ==492 &
                                     me1$unit==thissourceID]
  source.pred5 <- me1$chadefaux_s5[me1$month_id ==493 &
                                     me1$unit==thissourceID]
  source.pred6 <- me1$chadefaux_s6[me1$month_id ==494 &
                                     me1$unit==thissourceID]
  source.pred7 <- me1$chadefaux_s7[me1$month_id ==495 &
                                     me1$unit==thissourceID]
  source <- c(source, source.pred1,
              source.pred2,
              source.pred3,
              source.pred4,
              source.pred5,
              source.pred6,
              source.pred7
  )
  
  plot(1:12, source[1:12] , type='l', axes=F, xlab='', ylab='', xlim=c(1,19))
  lines(12:19,source[12:19] , type='l',  lty=3)
  
  if(this.unit=='cm') title(main = paste(df1$country_name[df1$unit==thissourceID][1], ': ', 08, '/', 2020, sep='')) 
  
  #yline(0, lty=2, col='grey')
  for(i in 1:3){
    bestTargetID <- as.numeric(as.character(ex$targetID[i]))
    bestTargetMonth <- as.numeric(as.character(ex$targetMonth_id[i]))
    bestName <- df1$country_name[df1$unit==bestTargetID][1]
    bestYear <- df1$year[df1$month_id==bestTargetMonth][1]
    bestMonth <- df1$month[df1$month_id==bestTargetMonth][1]
    best <- df1$output[df1$month_id %in% c((bestTargetMonth-11):(bestTargetMonth+7)) &
                           df1$unit==bestTargetID]
    bestFuture <- df1$output[  df1$month_id %in% c((bestTargetMonth+1)) &
                                 df1$unit==bestTargetID]
    
    plot(1:13,c(best[1:12],NA ), type='l', 
         col=2, axes=F, xlab='', ylab='',
         xlim=c(1,18), lwd=2,
         ylim=c(min(c(best, bestFuture)), max(c(best, bestFuture))))
    if(this.unit=='cm')title(main = paste(bestName, ': ', bestMonth, '/', bestYear, sep=''))
    lines(12:20, c(best[12:length(best)],bestFuture), lty=3, col='darkgrey', type='l', lwd=2)
    #lines(12:19, c(best[13:length(best)],bestFuture), lty=3, col='red', type='', lwd=2)
    #yline(0, lty=2, col='grey')
  }
}


dev.off()





findBestMatch <- function(this.id,
                          this.time, 
                          this.df, 
                          DVvar,
                          idvar,
                          timevar,
                          length.seq,
                          ncomparisons, nBestRows){
  source <- this.df[, DVvar][this.df[, idvar] == this.id &
                               this.df[, timevar] %in% (this.time-length.seq+1):this.time]
  possibleTargets <- this.df[this.df[, timevar] < (this.time - length.seq -1), ]
  
  RES <- NULL
  for(i in 1:ncomparisons){
    rn <- sample(1:nrow(possibleTargets), size = 1)
    this.target.id <- possibleTargets[,idvar][rn]
    this.target.time <- possibleTargets[, timevar][rn]
    this.target <- possibleTargets[, DVvar][possibleTargets[,idvar] == this.target.id &
                                              possibleTargets[, timevar] %in% (this.target.time-length.seq+1):this.target.time]
    this.target
    this.diss <- NULL
    try(this.diss <- diss(rbind(source,this.target), METHOD='DTWARP'), silent=T)
    if(length(this.diss!=0)){
      res <- data.frame(this.id = this.id, 
                        time.time = this.time, 
                        target.id = this.target.id, 
                        target.time =this.target.time,
                        distance = as.numeric(this.diss))
      RES <- rbind(RES, res) 
    }
  }
  
  RES <- RES[order(RES$distance), ]
  bestRows <- RES[1:nBestRows,]
  return(list( sourceSeq = source,
    bestMatchInfo  = bestRows,
               bestMatchSequences = df1[,DVvar][df1[,idvar]==bestRows$target.id & df1[,timevar] %in% (bestRows$target.time-length.seq+1):bestRows$target.time]))
}


bm <- findBestMatch(this.id = 79, 
                    this.time = 380, # this is the end time
                    this.df = df1,
                    DVvar = 'output',
                    idvar = 'unit',
                    timevar = 'month_id',
                    length.seq = 12,
                    ncomparisons = 100000,
                    nBestRows  = 1) # only works for 1, need to fix

plot(bm$sourceSeq, type='l')
plot(bm$bestMatchSequences, type='l')
dtw1 <- dtw(bm$sourceSeq, bm$bestMatchSequences)
pdf('../WriteUp/Figs/twoWayAlign.pdf')
par(mar=c(4,5,2,2))
dtwPlotTwoWay(dtw1, xts = bm$sourceSeq,
              yts =  bm$bestMatchSequences, 
              xlab='Month index', ylab='Change in fatalities', 
              las=1, cex.axis=2, match.col='red', cex=2, cex.lab=2, lwd=4, col=c('black', 'darkgrey'))

legend('topleft', 
       legend = c('Nigeria Aug. \'11', 'Pakistan May \'96'),
       lty=c(1,2), col=c(1,'darkgrey'), 
       cex=2, bty='n', lwd=3)
dev.off()

pdf('../WriteUp/Figs/threeWayAlign.pdf')
par(mar=c(7,7,2,2))
dtwPlotThreeWay(dtw1,  main='', lwd=3,
                xts = bm$sourceSeq, 
                yts =  bm$bestMatchSequences,
                xlab='',
                ylab = '', cex.axis=2
                  )
mtext('Pakistan \'96',2, line=5, cex=2)
mtext('Nigeria \'11',1, line=5, cex=2)
dev.off()

cor(bm$sourceSeq, bm$bestMatchSequences)




# Illustrate DTW:
library(dtw)
x = bm$sourceSeq
y = bm$bestMatchSequences


par(mfrow=c(2,1), mar=c(1,1,1,1))
plot(x, type='l', xlim=c())
plot(y, type='l')
xy = dtw(x,y,keep=T)
dtwPlotTwoWay(xy)
dtwPlotThreeWay(xy)
xy$directionMatrix
xy$distance
head(xy$costMatrix)
head(xy$localCostMatrix)
 xy$stepPattern

x
y

i1 = xy$index1[1:length(y)]
i2 = xy$index2[1:length(x)]
r= range(c(i1,i2))
s = r[1]:r[2]

pdf('../WriteUp/Figs/costMatrix.pdf')
par(mar=c(6,6,1,1))
ccm <- xy$localCostMatrix[s,s]
image(x=1:nrow(ccm),y=1:ncol(ccm),ccm,
      xlab='Nigeria indices', ylab = 'Pakistan indices',
      cex.lab=2, las=1, cex.axis=2)
text(row(ccm),col(ccm),
     label=round(ccm,1), cex=1.5)
lines(i1-r[1]+1, 
      i2-r[1]+1, lwd=5, col='blue')
dev.off()