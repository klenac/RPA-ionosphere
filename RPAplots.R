# Clear environment
rm(list=ls())

# Load required libraries
library(fNonlinear)
library(crqa)


myRPA<-function(day) {
  # set filenames 
  data_filename <- paste("mate",day,".Std",sep="")
  tecplot_filename <- paste(day,"_tec.png",sep="")
  gtecplot_filename <- paste(day,"_gtec.png",sep="")
  
  # set parameters for RPA plots (same for all days)
  delay = 1
  embed = 1
  rescale = 1
  radius = 1
  normalize = 0
  mindiagline = 2
  minvertline = 2
  tw = 0
  whiteline = FALSE
  recpt = FALSE
  side = "both"
  checkl = list(do = FALSE, thrshd = 3, datatype = "categorical", pad = TRUE)
  par = list(unit = 2, labelx = "Time", labely = "Time", cols = "red", pcex = 1)
  
  
  # load data
  data <- read.csv(data_filename, header = FALSE, sep = '')
  
  # Prepare data series
  tec <- data$V2
  gtec <- data$V3
  ts_tec <- as.ts(tec)
  ts_gtec <- as.ts(gtec)
  
  # calculate CRQA
  rpa_tec = crqa(ts_tec, ts_tec, delay, embed, rescale, radius, normalize, mindiagline,minvertline, tw, whiteline, recpt, side, checkl)
  rpa_gtec = crqa(ts_gtec, ts_gtec, delay, embed, rescale, radius, normalize, mindiagline, minvertline, tw, whiteline, recpt, side, checkl)
  
  # create tec plot as image in working dir
  png(tecplot_filename,width=3.25,height=3.25,units="in",res=1000,pointsize=3)
  RP = rpa_tec$RP
  plotRP(RP, par)
  dev.off()
  
  # create gtec plot as image in working dir
  png(gtecplot_filename,width=3.25,height=3.25,units="in",res=1000,pointsize=3)
  RP = rpa_gtec$RP
  plotRP(RP, par)
  dev.off()
} 




# Days
myRPA('076')
myRPA('130')
myRPA('251')
myRPA('301')
myRPA('302')





