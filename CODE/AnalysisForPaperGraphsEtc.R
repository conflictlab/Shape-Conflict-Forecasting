# find suitable examples etc.

pred <- read.csv('Results/PredictionsInViewsFormat/chadefaux_cm_444')
actual <- read_parquet('Data/df_cm_23Sept.parquet')
actual <- as.data.frame(actual)[, c('country_id', 'month_id', 'ln_ged_best_sb', 'in_africa')]
# create lags of dOutput
#df1 <- actual
actual$unit <- actual$country_id
# First, create lags of ln_ged_best_sb
print('... Calculating lags')
actual <- actual[order(actual$unit, actual$month_id), ]
for(this.lag in 0:12){
  laggedDVname <- paste('ln_ged_best_sb.l', this.lag, sep=''  )
  laggedDVvalue <- unlist(tapply(actual$ln_ged_best_sb, INDEX = actual$unit, FUN=function(x)c(rep(NA, this.lag), x[1:(length(x)-this.lag)]) ))
  actual[, laggedDVname] <- laggedDVvalue
}

for(that.lag in 0:11){
  laggedDVname <- paste('dln_ged_best_sb.l', that.lag, sep='')
  laggedDVvalue <- actual[, paste('ln_ged_best_sb.l', that.lag, sep='')] - actual[, paste('ln_ged_best_sb.l', that.lag+1, sep='')]
  actual[, laggedDVname] <- laggedDVvalue
}

m1 <- merge(pred, actual, 
            by.x=c('unit_id', 'month_id'),
            by.y = c('country_id', 'month_id'))

# find best match
this.mat <- data.frame(chadefaux_s2=m1$chadefaux_s2,
                       dln_ged_best_sb = m1$dln_ged_best_sb.l1,
                       unit_id = m1$unit_id,
                       month_id = m1$month_id,
                       in_africa =m1$in_africa)

# FInd an example where source and target match well. 
#NB: this is rough--needs rewriting. Not used
if(1==2){
  calcResults <- function(t1){
    #if(t1%%10000 == 0) 
    print(t1)
    source <- this.mat[t1:(t1+11), 1]
    if(sd(source, na.rm = T)!=0) source <- (source - mean(source))/sd(source)
    if(sd(source, na.rm = T)==0) source <- (source - mean(source))
    target <-  this.mat[t1:(t1+11), 2]
    if(sd(target, na.rm=T)!=0)target <-  (target - mean(target))/sd(target)
    if(sd(target, na.rm=T)==0)target <-  (target - mean(target))
    
    this.diss <- as.numeric(diss(rbind(source, target), METHOD = 'DTWARP'))
    df1 <- data.frame(this.mat$unit_id[t1],
                      this.mat$month_id[t1],
                      this.diss,
                      this.mat$in_africa[t1])
    results <- rbind(results, df1)
    #return(results)
    write.table(results, 'Results/Illustrations/matches.csv', sep=',', append =T, col.names = F)
  }
  
  results <- NULL
  for(t1 in 1:nrow(this.mat)){
    try(calcResults(t1), silent = F, )
  }
  
  results <- read.csv('Results/Illustrations/matches.csv')
  colnames(results) <- c('unit_id', 'month_id', 'this.diss', 'in_africa')
  results <- results[results$this.diss!=0,]
  results <- results[order(results$this.diss), ]
}




# LINE GRAPHS FOR SELECTED LOCATIONS + Africa

## Entire continent predictions
PREDS <- NULL
for(i in 1:1000){
  samp1 <- m1
  samp1 <- samp1[sample(1:nrow(samp1), nrow(samp1), replace=T), ]
  ag2 <- mean(samp1$chadefaux_s2[samp1$month_id==490 & samp1$in_africa==1], na.rm=T)
  ag3 <- mean(samp1$chadefaux_s3[samp1$month_id==491 & samp1$in_africa==1], na.rm=T)
  ag4 <- mean(samp1$chadefaux_s4[samp1$month_id==492 & samp1$in_africa==1], na.rm=T)
  ag5 <- mean(samp1$chadefaux_s5[samp1$month_id==493 & samp1$in_africa==1], na.rm=T)
  ag6 <- mean(samp1$chadefaux_s6[samp1$month_id==494 & samp1$in_africa==1], na.rm=T)
  ag7 <- mean(samp1$chadefaux_s7[samp1$month_id==495 & samp1$in_africa==1], na.rm=T)
  preds <- c(ag2, ag3, ag4, ag5, ag6, ag7)
  PREDS <-rbind(PREDS, preds)
}

PREDS.lo <- apply(PREDS, 2, FUN=function(x)quantile(x, 0.025, na.rm = T))
PREDS.hi <- apply(PREDS, 2, FUN=function(x)quantile(x, 0.975, na.rm = T))
PREDS.med <- apply(PREDS, 2, FUN=function(x)quantile(x, 0.5, na.rm = T))

pdf('Results/Figs/overallAfrica.pdf')
par(mar=c(8,8,1,1))
plot(c(490:495), PREDS.med, 
     type='l', ylim=c(0,0.11),
     ylab = '',
     xlab = '', las=1, axes=F)
axis(1, at=490:495, cex.axis=2,
     c('Oct. 20', 'Nov. 20', 'Dec. 20',
       'Jan. 21', 'Feb. 21', 'Mar. 21'),
     las=2)
axis(2, at=c(0, 0.05, 0.1), cex.axis=2,
     c(0, 0.05, 0.1), las=1)
#mtext('Month', 1, cex=2, line = 2)
mtext('Predicted change', 2, cex=2.5, line=5)

polygon(c(490:495,rev(490:495)),
        c(PREDS.hi,rev(PREDS.lo)),
        col = "grey85", border = FALSE)
lines(c(490:495), PREDS.hi, type='l', lty=2, lwd=3)
lines(c(490:495), PREDS.lo, type='l', lty=2, lwd=3)
lines(c(490:495), PREDS.med, lwd=3.5, type='b')
dev.off()



# Indiv. preds:

pdf('Results/Figs/PredsSelectedExamples.pdf')
par(mar=c(8,8,1,1), bty='n')
col.marker <- 0
mycols <- c('black', 'darkgrey', 'lightgrey')
mypch <- 16:18
plot(c(490:495), seq(-0.15,0.1, length.out=6), ylim=c(-0.15,0.1),
     type='n',
     ylab = '',
     xlab = '', las=1, xaxt='n', lwd=3, cex.axis=2)
axis(1, at=490:495, cex.axis=2,
     c('Oct. 20', 'Nov. 20', 'Dec. 20',
       'Jan. 21', 'Feb. 21', 'Mar. 21'),
     las=2)
legend(x = 491, y=0.035, cex=2.5, bty='n', 
       legend = c('Cameroon', 'Mozambique', 'Egypt'), 
       lwd=4, lty=1, col=mycols, pch=mypch)

#mtext('Month', 1, cex=2, line = 2)
mtext('Predicted change', 2, cex=2.5, line=5)

for(unitID in c(69, 162, 222)){
  col.marker <- col.marker +1
  PREDS <- NULL
  for(i in 1:1000){
    samp1 <- m1[m1$unit_id==unitID,]
    samp1 <- samp1[sample(1:nrow(samp1), nrow(samp1), replace=T), ]
    ag2 <- mean(samp1$chadefaux_s2[samp1$month_id==490 ], na.rm=T)
    ag3 <- mean(samp1$chadefaux_s3[samp1$month_id==491 ], na.rm=T)
    ag4 <- mean(samp1$chadefaux_s4[samp1$month_id==492 ], na.rm=T)
    ag5 <- mean(samp1$chadefaux_s5[samp1$month_id==493 ], na.rm=T)
    ag6 <- mean(samp1$chadefaux_s6[samp1$month_id==494 ], na.rm=T)
    ag7 <- mean(samp1$chadefaux_s7[samp1$month_id==495 ], na.rm=T)
    preds <- c(ag2, ag3, ag4, ag5, ag6, ag7)
    PREDS <-rbind(PREDS, preds)
  }
  
  PREDS.lo <- apply(PREDS, 2, FUN=function(x)quantile(x, 0.025, na.rm = T))
  PREDS.hi <- apply(PREDS, 2, FUN=function(x)quantile(x, 0.975, na.rm = T))
  PREDS.med <- apply(PREDS, 2, FUN=function(x)quantile(x, 0.5, na.rm = T))
  
  lines(c(490:495), PREDS.med, 
        ylim=c(-0.15,0.1), type='b',
        col=mycols[col.marker], lwd=4,
        pch=mypch[col.marker], cex=3)
  
}
dev.off()