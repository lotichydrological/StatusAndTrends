run_seaKen <- function (df.all) {
  sea_ken_int <- data.frame(Station_ID=sort(unique(df.all$Station_ID)),analyte="none",slope="none",pvalue="none",median="none",N="none",stringsAsFactors=FALSE)
  for (p in 1:length(unique(df.all$Analyte))) {
    parm <- unique(df.all$Analyte)[p]
    
    for(ii in 1:length(sea_ken_int$Station_ID)) { 
      # specifiy current Station_ID
      tmp.one.station <- sea_ken_int$Station_ID[ii]
      tmp.data.raw <- df.all[df.all$Station_ID == tmp.one.station & df.all$Analyte == parm,]
      sea_ken_int$analyte[ii] <- parm
      sea_ken_int$N[ii] <- length(tmp.data.raw$Result)
      if (!nrow(tmp.data.raw) > 1) next
      # Reshape and manipulate data to convert to wqData-class
      tmp.data <- data.frame(date=tmp.data.raw$Sampled,
                             time="0000",
                             stn=as.character(tmp.one.station),
                             depth=1,
                             variable=parm,
                             value=suppressWarnings(as.numeric(tmp.data.raw$Result)), 
                             stringsAsFactors=FALSE)
      
      # Construct an object of class "WqData"
      tmp.wq <- wqData(tmp.data, c(1,3,4), c(5,6), site.order = TRUE, type = "long",time.format = "%Y-%m-%d %H:%M:%S")
      # Create time series from water quality data
      tmp.ts <- suppressWarnings(tsMake(tmp.wq, focus = parm, layer = c(0, 5)) )
      if (!length(tmp.ts) > 2 |
          start(tmp.ts)[1] == end(tmp.ts)[1] | 
          !any(1:frequency(tmp.ts) %in% cycle(tmp.ts)) |
          !all(1:12 %in% cycle(tmp.ts))) next
      tmp.result <- seaKen(tmp.ts)
      sea_ken_int$pvalue[ii] <- tmp.result$p.value
      sea_ken_int$slope[ii] <- tmp.result$sen.slope
      sea_ken_int$median[ii] <- suppressWarnings(median(as.numeric(tmp.data.raw$Result),na.rm = FALSE))
      
      rm(list=ls(pattern="tmp.*"))
    }
    
    ifelse(p == 1, SeaKen <- sea_ken_int, SeaKen <- rbind(SeaKen, sea_ken_int))
  }
  
  SeaKen$signif <- ifelse(SeaKen$pvalue<=0.01, "99% Significance Level",
                          ifelse(SeaKen$pvalue<=0.05, "95% Significance Level",
                                 ifelse(SeaKen$pvalue<=0.1, "90% Significance Level",
                                        ifelse(SeaKen$pvalue<=0.2, "80% Significance Level","Not Significant"))))
  
  return(SeaKen)
}
