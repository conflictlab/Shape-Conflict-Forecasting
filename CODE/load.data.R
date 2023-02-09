

load.data <- function(step, this.unit){
  # Import data. Data was exported from python notebook
  # NB Prior step needed: export parquet file from Benchmark notebook provided. Now import it into R, and transform into standard dataframe:
  
  df1 <- read_parquet(paste('Data/df_', this.unit, '_23Sept.parquet', sep=''))
  #write_csv(df1, paste('Data/df_', this.unit, '_23Sept.csv', sep=''))
  df1 <- as.data.frame(df1)
  if(this.unit == 'pgm') df1$unit <- df1$pg_id
  if(this.unit == 'cm') df1$unit <- df1$country_id
  
  
  #------ CREATE LAGS OF DVs ----------------------------------------------
  # First, sort the data so the "apply" function below works properly. DO NOT remove
  print('... Calculating lags')
  df1 <- df1[order(df1$unit, df1$month_id), ]
  
  # First, create lags of ln_ged_best_sb
  for(this.lag in 0:12){
    laggedDVname <- paste('ln_ged_best_sb.l', this.lag, sep=''  )
    laggedDVvalue <- unlist(tapply(df1$ln_ged_best_sb, INDEX = df1$unit, FUN=function(x)c(rep(NA, this.lag), x[1:(length(x)-this.lag)]) ))
    df1[, laggedDVname] <- laggedDVvalue
  }
  
  # create lags of dOutput
  for(that.lag in 0:11){
    laggedDVname <- paste('dln_ged_best_sb.l', that.lag, sep='')
    laggedDVvalue <- df1[, paste('ln_ged_best_sb.l', that.lag, sep='')] - df1[, paste('ln_ged_best_sb.l', that.lag+1, sep='')]
    df1[, laggedDVname] <- laggedDVvalue
  }
  
  
  
  #------DEFINE DEPENDENT VARIABLE ----------------------------------------------
  
  
  df1$ln_ged_best_sb.next <- unlist(tapply(df1$ln_ged_best_sb, INDEX = df1$unit, FUN=function(x)c(x[(step+1):(length(x))], rep(NA, step) )))
  df1$dln_ged_best_sb.next <- df1$ln_ged_best_sb.next - df1$ln_ged_best_sb
  df1$dln_ged_best_sb <- df1$ln_ged_best_sb - log1p(df1$tlag_1_ged_best_sb)
  
  # lag the DV by the number of steps. As a result, we can train on period t = [0, end-steps+1] and test on t =`steps'
  df1$output <- df1$dln_ged_best_sb #df1[, paste('dln_ged_best_sb.l', step-1, sep='') ] # my sequences will be based on these
  df1$future <- df1$dln_ged_best_sb.next # these will be the future of the sequences. I.e., what we expect to happen 
  
  return(df1)
}
