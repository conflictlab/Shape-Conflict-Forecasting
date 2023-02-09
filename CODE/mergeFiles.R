# Merge all the predictions. NB: irrelevant if not doing the ViEWS competition

print(this.unit)
df1 <- read_parquet(paste('Data/df_', this.unit, '_23Sept.parquet', sep=''))
df1 <- as.data.frame(df1)
if(this.unit == 'pgm') df1$unit <- df1$pg_id
if(this.unit == 'cm') df1$unit <- df1$country_id

for(this.set in c(408, 444, 487)){
  print(this.set)
  master.file <- expand.grid(unit_id=unique(df1$unit), month_id= unique(df1$month_id))
  for(this.step in 2:7){
    this.file <- read.csv(paste('Results/PostPredictions/predictionsChadefaux_', this.unit, '_s', this.step, '_', this.set, '.csv', sep=''))
    this.file <- this.file[, c('unit_id', 'month_id', paste('chadefaux_s', this.step, sep=''))]  # use grep here
    master.file <- merge(master.file, this.file, by=c('unit_id', 'month_id'), all.x=T)
  }
  head(master.file[master.file$month_id==408,])
  master.file <- master.file[master.file$month_id >= this.set,]
  write.csv(master.file, paste('Results/PredictionsInViewsFormat/chadefaux', this.unit, this.set,  sep='_'))
}
