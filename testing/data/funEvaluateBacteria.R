#From SO: http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE, zero.propagate = TRUE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

Evaluate30dayEcoli <- function(df.all, parameter, station) {
  sub <- df.all[df.all$Analyte == parameter &
                  df.all$Station_ID == strsplit(station," - ")[[1]][1],]
  
  sub_start <- df.all[df.all$Analyte == input$selectParameter,]
  
  gm_df <- data.frame()
  for (i in 1:length(unique(sub_start$Station_ID))) {
    sub <- sub_start[sub_start$Station_ID == unique(sub_start$Station_ID)[i],]
    
    sort(sub[,'Sampled'])
    
    sub$day <- as.Date(sub$Sampled, format = "%Y-%m-%d")
    
    day <- as.Date((seq(min(sub$Sampled),max(sub$Sampled),by=86400)), format = "%Y-%m-%d")
    Result <- rep(NA, length(day))
    sub_long <- rbind(sub[,c('day','Result')], data.frame(day, Result))
    
    sub_long_max <- aggregate(sub_long, by = list(sub_long$day), FUN = function(x) {if(all(is.na(x))) {
      NA
    } else {
      max(x, na.rm = TRUE)}
    })
    
    sub_long_max$n <- ifelse(is.na(sub_long_max$Result), 0, 1)
    
    sub_long_max$ind <- rownames(sub_long_max)
    
    if (nrow(sub_long_max) > 1) {
      obs_in_30 <- rollapplyr(sub_long_max$n, width = 30, FUN = sum)
      
      #print(paste(any(obs_in_30 >= 5),unique(sub[,'Station_ID'])))
      
      
      
      geo_mean_30 <- rollapplyr(sub_long_max$Result, width = 30, FUN = gm_mean)
      
      gm_df_all <- data.frame("day" = sub_long_max[30:nrow(sub_long_max),'day'],
                              "n" = obs_in_30,
                              "gm" = geo_mean_30,
                              "ind" = rep(NA, length(geo_mean_30)))
      
      for (j in 1:nrow(gm_df_all)) {
        sub_sub <- sub_long_max[(which(sub_long_max$day == gm_df_all[j,'day'])-29):which(sub_long_max$day == gm_df_all[j,'day']),]
        gm_df_all[j,'ind'] <- paste(sub_sub[which(sub_sub$Result != 0),'ind'],collapse = ",")
      }
      
      gm_df_min5 <- gm_df_all[gm_df_all$n >= 5,]
      
      gm_df_5_first <- gm_df_min5[!duplicated(gm_df_min5$ind),]
      
      if (nrow(gm_df_5_first) > 0) {
        gm_df_5_first$id <- unique(sub_start$Station_ID)[i]
        
        gm_df <-  rbind(gm_df, gm_df_5_first)
      }
      
    } else {
      #print(paste("FALSE",unique(sub[,'Station_ID'])))
    }
    
  }
  
  gm_df <- within(gm_df, rm(ind))
  
  return(gm_df)
}

