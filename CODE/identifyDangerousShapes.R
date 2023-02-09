# Identify dangerous shapes
library(readr)
library(arrow)



# Load Data
pred <- read.csv('Results/PostPredictions/predictionsChadefaux_cm_s2_408.csv')
actual <- read_parquet('Data/df_cm_23Sept.parquet')
actual <- as.data.frame(actual)[, c('country_name', 'year', 'month', 'country_id', 'month_id', 'ln_ged_best_sb', 'in_africa')]
# create lags of dOutput
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


# Create a simple data frame
this.mat <- data.frame(chadefaux_s2=m1$chadefaux_s2,
                       dln_ged_best_sb = m1$dln_ged_best_sb.l1,
                       unit_id = m1$unit_id,
                       month_id = m1$month_id,
                       in_africa =m1$in_africa)


# Find historical cases in which I predicted well
interestingCases <- which(abs(this.mat$chadefaux_s2) > 0.2 &
                            abs(this.mat$dln_ged_best_sb - this.mat$chadefaux_s2)< 0.2 & 
                            this.mat$dln_ged_best_sb!=0)
reallyInterestingCases <- c(8784, 16489)
pdf('../../WriteUp/Figs/historicalCases.pdf')
{
par(pty='s', mar=c(0,0,0,0), mfrow=c(2,6))
for(ic in reallyInterestingCases){
  myseq <- this.mat[ic,]
  # find corresponding sequence:
  this.seq <- actual$dln_ged_best_sb.l1[actual$country_id==myseq$unit_id & 
                                          actual$month_id%in%(myseq$month_id-12):(myseq$month_id-1)]
  
  this.seq.extended <- actual$dln_ged_best_sb.l1[actual$country_id==myseq$unit_id & 
                                                   actual$month_id%in%(myseq$month_id-12):(myseq$month_id+3)]
  #plot(1:length(this.seq.extended), this.seq.extended, type='l')
  #lines(1:length(this.seq.extended), c(this.seq, rep(NA,4)), col=2, type='l')
  #points(13, myseq$chadefaux_s2, col=3)
  #points(13, myseq$dln_ged_best_sb, col=4)

  #actual$country_name[actual$country_id==myseq$unit_id]
  #actual$year[actual$month_id==myseq$month_id]
  #actual$month[actual$month_id==myseq$month_id]
  
  #Find similar shapes
  file.remove('illustrations/closestSequences.csv')
  findCloseMatch <- function(i){
    if(i%%10000==0) print(i)
    (rm(this.diss, target))
    target <- actual$dln_ged_best_sb.l1[(i-12):(i-1)]
    target.pred <- actual$dln_ged_best_sb.l1[i]
    (this.diss <- as.numeric(diss(rbind(this.seq, target), METHOD = 'DTWARP')))
    
    write.table(data.frame(matchedObservation  = i, this.diss, thisobsfuture = target.pred),
                file = 'illustrations/closestSequences.csv', append=T, sep=',', row.names = F, col.names = F)
  }
  actual <- actual[order(actual$country_id, actual$month_id),]
  for(i in 13:nrow(actual)){
    try(findCloseMatch(i), silent = TRUE)
  }
  
  
  cs <- read.csv('illustrations/closestSequences.csv')
  colnames(cs) <- c('obsN', 'distance')
  cs <- cs[order(cs$distance),]
  
  
  #plot the original sequence
  
  plot(this.seq, col=1, type='l', xlim=c(1, 13),
       axes=F, xlab='', lwd=2)
  lines(12:13, this.seq.extended[12:13], col=2, type='l', lty=2, lwd=2)
  title(paste(actual$country_name[actual$country_id==myseq$unit_id][1], '\n', month.abb[actual$month[actual$month_id==myseq$month_id][1]], actual$year[actual$month_id==myseq$month_id][1] ), line=1)
  
  # then plot the close matches.
  index <- 0
  i=1
  while(index < 5 ){
    whichobs <- cs$obsN[i]
    if(actual$month_id[whichobs] < myseq$month_id & actual$country_id[whichobs]!=myseq$unit_id){
      
      plot(actual$dln_ged_best_sb.l1[(whichobs-12):(whichobs-1)],
           type='l', xlim=c(1,13), axes=F,main='',
           xlab='', ylab='', lwd=2)
      title(paste(actual$country_name[whichobs], '\n', month.abb[actual$month[whichobs]], actual$year[whichobs] ), line=1)
      lines(12:13,c(actual$dln_ged_best_sb.l1[whichobs-1],actual$dln_ged_best_sb.l1[whichobs]), col='darkgrey', lty=2, lwd=2)
      points(13, actual$dln_ged_best_sb.l1[whichobs],  col='darkgrey', cex=2)
      index <- index+1
    }
    i=i+1
  }
}
}
dev.off()


# SHAPES ASSOCIATED WITH CONFLICT ESCALATION/DEESCALATION (try a different method)
library(TSclust)
library(dtwclust)
# PNOTE: Look in this object for the obs with the highest risk. Then go look for their shapes
# and Compare these shapes with the least dangerous
this.mat <- this.mat[!is.na(this.mat$dln_ged_best_sb),]
distance.method <- 'dtw'
centroid.method <- 'mean'
# Try medoids:
library(BBmisc)
par(mfrow=c(2,6), pty='s', mar=c(0,0,0,0))
this.mat <- this.mat[!duplicated(this.mat), ]
nseqs <- round(nrow(this.mat)*5/100)
nclusters <- 10
set.seed(0)
for(this.type in c('escalating', 'deescalating')){
  # If I want to look at most dangerous:
  if(this.type=='escalating') {
    this.mat <- this.mat[rev(order(this.mat$chadefaux_s2)), ]
    plot(0, axes=F, type='n', xlab='', ylab='')
    text(x = 1, y=0, labels='Increase',cex= 1.5 )
  }
  # If I want to look at least dangerous:
  if(this.type=='deescalating') {
    this.mat <- this.mat[(order(this.mat$chadefaux_s2)), ]
    plot(0, axes=F, type='n', xlab='', ylab='')
    text(x = 1, y=0, labels='Decrease', cex = 1.4 )
  }
  seqs <- NULL
  #look at the top 500 sequences (for example)). Collect tehm into "seqs"
  for(i in 1:nseqs){
    this.seq <- actual$ln_ged_best_sb[actual$country_id==this.mat$unit_id[i] & 
                                        actual$month_id%in%(this.mat$month_id[i]-11):this.mat$month_id[i]]
    seqs <- rbind(seqs, this.seq)  
  }
  
  # Extract the shape of these 500 (or whatever) sequences
  
  seqs.norm <- BBmisc::normalize(seqs, method="standardize")
  clust.pam <- tsclust(seqs.norm,
                       type="partitional", 
                       k = nclusters, 
                       distance=distance.method,
                       centroid=centroid.method)
  
  # Plot the sequences
  plot(clust.pam,
       type = "centroids"
  )
  
  pdf(paste('../../WriteUp/Figs/most', this.type,'Shapes', nclusters,'clusters', distance.method,'_',centroid.method,'.pdf', sep=''))
  par(mfrow=c(2,nclusters/2), mar=c(1,1,1,1), pty='s')
  this.order <- rev(order(clust.pam@clusinfo$size))
  for(i in this.order){
    plot(clust.pam@centroids[[i]], type='l', 
         axes=F,xlab='', ylab='', lwd=2, 
         main=paste(round(100*clust.pam@clusinfo$size[i]/nseqs,0), '%', sep=''))
  }
  dev.off()
}







# WHAT ARE THE MOST *COMMON* SHAPES?

set.seed(0)
distance.method <- 'dtw'
centroid.method <- 'mean'
seqs <- NULL
for(i in 1:nrow(this.mat)){
  this.seq <- actual$ln_ged_best_sb[actual$country_id==this.mat$unit_id[i] & 
                                      actual$month_id%in%(this.mat$month_id[i]-11):this.mat$month_id[i]]
  seqs <- rbind(seqs, this.seq)  
}

seqs.norm <- BBmisc::normalize(seqs, method="standardize")

# Cluster into k groups
nclusters <- 10L

nseqs <- 2000
clust.pam <- tsclust(seqs.norm[sample(1:nrow(seqs.norm), nseqs),],
                     type="partitional", 
                     k = nclusters, 
                     distance=distance.method,
                     centroid=centroid.method)
# Plot the sequences
plot(clust.pam,
     type = "centroids",
     lwd=0.5, labs.arg = list(title=''))

pdf(paste('../../WriteUp/Figs/mostCommonShapes_', distance.method, '.pdf', sep=''))
par(mfrow=c(2,5), mar=c(1,1,1,1), pty='s')
this.order <- rev(order(clust.pam@clusinfo$size))
for(i in this.order){
  plot(clust.pam@centroids[[i]], type='l', 
       axes=F,xlab='', ylab='', lwd=2, 
       main=paste(round(100*clust.pam@clusinfo$size[i]/nseqs,0), '%', sep=''))
}
dev.off()

# Result
#matplot(t(seqs.norm[1:50,]),
#        type = "l", col = 1:50)
#points(sh1, col=2)


plot(clust.pam,
     type = "centroids",
     lwd=0.5)

clust.pam@cluster
#t.test(clust.pam@cluster[1:n],
#       clust.pam@cluster[(n+1):(2*n)])
t.test(clust.pam@cluster[samp1 < 1000],
       clust.pam@cluster[samp1 >16000])




##########################


# OLD CODE, abandoned

if(1==2){
  # SHAPES ASSOCIATED WITH CONFLICT ESCALATION/DEESCALATION
  library(TSclust)
  library(dtwclust)
  # PNOTE: Look in this object for the obs with the highest risk. Then go look for their shapes
  # and Compare these shapes with the least dangerous
  this.mat <- this.mat[!is.na(this.mat$dln_ged_best_sb),]
  
  # Try medoids:
  library(BBmisc)
  pdf('../../WriteUp/Figs/dangerousShapes.pdf')
  {
    par(mfrow=c(2,6), pty='s', mar=c(0,0,0,0))
    this.mat <- this.mat[!duplicated(this.mat), ]
    for(this.order in 1:2){
      # If I want to look at most dangerous:
      if(this.order==1) {
        this.mat <- this.mat[rev(order(this.mat$chadefaux_s2)), ]
        plot(0, axes=F, type='n', xlab='', ylab='')
        text(x = 1, y=0, labels='Increase',cex= 1.5 )
      }
      # If I want to look at least dangerous:
      if(this.order==2) {
        this.mat <- this.mat[(order(this.mat$chadefaux_s2)), ]
        plot(0, axes=F, type='n', xlab='', ylab='')
        text(x = 1, y=0, labels='Decrease', cex = 1.4 )
      }
      ncuts <- 5
      # Look at the first n observations
      for(n in seq(1, 100, 100/ncuts)){
        
        seqs <- NULL
        #look at the n:n+50 sequences (for example)). Collect tehm into "seqs"
        for(i in c(n:(n + 100/ncuts))){
          this.seq <- actual$ln_ged_best_sb[actual$country_id==this.mat$unit_id[i] & 
                                              actual$month_id%in%(this.mat$month_id[i]-11):this.mat$month_id[i]]
          seqs <- rbind(seqs, this.seq)  
        }
        
        # Extract the shape of these 50 (or whatever) sequences
        sh1 <- shape_extraction(seqs, znorm=T)
        # Plot the extracted shape
        plot(sh1, type='l', axes=F, xlab='', ylab='', lwd=2,
             main = round(mean(this.mat$chadefaux_s2[n:(n+100/ncuts)]),2))
      }
    }
  }
  dev.off()
}


if(1==2){
  par(mfrow=c(4,5), mar=c(0,0,1,0))
  # What do the 10% most dangerous sequences look like? How about the next 10% most dangerous... until the safest 10%?
  this.mat <- this.mat[rev(order(this.mat$chadefaux_s2)),]
  highs <- meanHighs <- medianHighs <- NULL
  incr <- round(nrow(this.mat)/20)
  for(index in seq(1, nrow(this.mat)-1, incr)){
    print(index)
    for(i in index:(index+incr)){
      this.seq <- actual$ln_ged_best_sb[actual$country_id==this.mat$unit_id[i] & 
                                          actual$month_id%in%(this.mat$month_id[i]-11):this.mat$month_id[i]]
      #plot(this.seq, type='l')
      highs <- rbind(highs, this.seq)  
    }
    highs <- as.matrix(highs)
    highs2 <- t(apply(highs, 1,
                      FUN = function(x)(x-mean(x, na.rm=T)) / sd(x, na.rm=T))
    )
    
    meanHighs <- apply(highs2, 2, function(x)mean(x, na.rm=T))
    medianHighs <- apply(highs2, 2, function(x)median(x, na.rm=T))
    plot(meanHighs, 
         ylim=c(-0.4,0.5), 
         type='l',
         #ylim=c(min(medianHighs),max(medianHighs)),
         main = paste('Risk = ', round(mean(this.mat$chadefaux_s2[index:(index+incr)], na.rm=T),3)))                 
    library(graphics)
  }
  
  # boxplot.matrix(highs2, use.cols = T, outline=F)
  
  
  
  
  seqs.norm <- BBmisc::normalize(seqs, method="standardize")
  # If I want to test the best number of clusters:
  if(1==2){
    clust.pam <- tsclust(seqs.norm, type="partitional", 
                         k=2L:6L, distance="dtw",
                         centroid="pam")
    cvis <- lapply(X = clust.pam,
                   FUN=function(x)cvi(x,
                                      b = c(rep(1,n), rep(2,n)),
                                      type='valid'))
    cvis.mat <- matrix(unlist(cvis), ncol = 5, byrow = FALSE)
    plot(cvis.mat[1,])
  }
  
  #clust.pam <- tsclust(seqs.norm, type="partitional", 
  #                     k=2L, distance="dtw",
  #                     centroid="pam")
  
  
  # Result
  #matplot(t(seqs.norm[1:50,]),
  #        type = "l", col = 1:50)
  #points(sh1, col=2)
  
  
  plot(clust.pam, type = "centroids")
  
  clust.pam@cluster
  #t.test(clust.pam@cluster[1:n],
  #       clust.pam@cluster[(n+1):(2*n)])
  t.test(clust.pam@cluster[samp1 < 1000],
         clust.pam@cluster[samp1 >16000])
  
  
  
  
  
  
  
  
  
  splitTimesOfSequencesPasts <- lapply(timesOfsequencesPasts, FUN=function(x)strsplit(x, split = '_'))
  dates <- data.frame(year=as.numeric(as.character(unlist(lapply(splitTimesOfSequencesPasts, FUN=function(x)unlist(x)[5])))),
                      month=as.numeric(as.character(unlist(lapply(splitTimesOfSequencesPasts, FUN=function(x)unlist(x)[7])))),
                      units=as.numeric(as.character(unlist(lapply(splitTimesOfSequencesPasts, FUN=function(x)unlist(x)[9])))))
  
  #months don't work, so need to merge with months elsewhere 
  #df1 <- read_csv(paste('Data/df_', this.unit, '_23Sept.csv', sep=''))    
  df1 <- read_parquet(paste('Data/df_', this.unit, '_23Sept.parquet', sep=''))
  df1 <- as.data.frame(df1)
  df2 <- df1[,c('year', 'month', 'month_id')]
  df2 <- df2[!duplicated(df2),]
  df3 <- merge(df2, dates,
               by=c('year', 'month'),
               all.y=T, all.x=F)
  
  this.i <- 2
  for(this.i in 1:1000){
    uoi <- which(df3$units == this.mat$unit_id[this.i] &
                   df3$month_id == this.mat$month_id[this.i])
    
    print(df1$ln_ged_best_sb[df1$country_id==this.mat$unit_id[2] & 
                               df1$month_id==this.mat$month_id[2]])
  }
  
}