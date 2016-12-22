plot.DO<-function(new_data,
                  df.all,
                  selectUseDO = input$selectUseDO,
                  selectSpawning = input$selectSpawning,
                  analyte_column = 'Analyte',
                  station_id_column = 'Station_ID',
                  station_desc_column = 'Station_Description',
                  datetime_column = 'Sampled',
                  result_column = 'Result',
                  datetime_format = '%Y-%m-%d %H:%M:%S',
                  parm) {
  require(ggplot2)
  #dataframe that assigns WQS values to Aquatic Life Uses
  new_data$Sampled <- as.POSIXct(strptime(new_data[, datetime_column],
                                          format = datetime_format))
  #new_data$Result<-as.numeric(new_data$Result)
  x.min <- min(new_data$Sampled)
  x.max <- max(new_data$Sampled)
  x.lim <- c(x.min, x.max)
  y.min <- floor(min(new_data[, result_column]))
  y.max <- ceiling(max(new_data[, result_column]))
  y.lim <- c(y.min, y.max)
  title <- paste0(min(new_data[, station_desc_column]), ", ID = ",
                  min(new_data[, station_id_column]))
  x.lab <- "Date"
  y.lab <- parm
  
  
  ##Generate Exceedances of WQS##
  new_data$selectUseDO<-selectUseDO
  
  spd_list <- strsplit(selectSpawning, split = "-")
  spd_chron <- lapply(spd_list, function(x) {as.chron(x, format = "%B %d")})
  spd_months <- lapply(spd_chron, months)
  spd_days <- lapply(spd_chron, days)
  spd_months_num <- lapply(spd_months, as.numeric)
  spd_days_num <- lapply(spd_days, as.numeric)
  SSTART_MONTH <- unlist(lapply(spd_months_num, function(x) x[1]))
  SEND_MONTH <- unlist(lapply(spd_months_num, function(x) x[2]))
  SSTART_DAY <- unlist(lapply(spd_days_num, function(x) x[1]))
  SEND_DAY <- unlist(lapply(spd_days_num, function(x) x[2]))
  sdata <- as.data.frame(cbind(SSTART_MONTH, SSTART_DAY, SEND_MONTH, SEND_DAY))

sdata$Station_ID <- unique(new_data$Station_ID)
sdata$aqu_use_des <- selectUseDO
sdata$numcrit<- if(selectUseDO == 'Cold-Water Aquatic Life') {
  8
} else if (selectUseDO == 'Cool-Water Aquatic Life') {
  6.5
} else if (selectUseDO == 'Warm-Water Aquatic Life') {
  5.5
} else if (selectUseDO == 'Estuarine Waters') {
  6.5
}
new_data$sdata <- match(new_data[, 'Station_ID'],
                        sdata[, 'Station_ID'])
new_data$cdate <- lubridate::month(new_data$Sampled) + lubridate::day(new_data$Sampled)*.01
new_data$sstr <- as.numeric(sdata$SSTART_MONTH[new_data$sdata]) +
  (as.numeric(sdata$SSTART_DAY[new_data$sdata]) *.01)
new_data$send <- as.numeric(sdata$SEND_MONTH[new_data$sdata]) +
  (as.numeric(sdata$SEND_DAY[new_data$sdata]) *.01)
new_data$bioc <- as.numeric(sdata$numcrit[new_data$sdata])
## checks to see if there is an over winter spawning period
new_data$winter <- ifelse(new_data$send < new_data$sstr, TRUE, FALSE)
## looks up the summer bio criterion and spawning start end/date and returns TRUE/FALSE if current date is in summer or spawning period
new_data$bioc <- ifelse(is.na(new_data$winter), new_data$bioc, ifelse(
  new_data$winter == TRUE,
  ifelse(new_data$sstr <= new_data$cdate | new_data$send >= new_data$cdate,
         11, new_data$bioc),
  ifelse(new_data$sstr <= new_data$cdate & new_data$send >= new_data$cdate,
         11, new_data$bioc)))
#Merge %DO with [DO]##
DOsat<-df.all%>%
  filter(Analyte == 'Dissolved oxygen saturation')
DOsat$Sampled<-as.POSIXct(strptime(DOsat[, datetime_column],
                                   format = datetime_format))
DOsat$id<-paste(DOsat$Station_ID, DOsat$Sampled, sep=" ")
new_data$id<-paste(new_data$Station_ID, new_data$Sampled, sep=" ")
#Result.y = results from %DO; Result.x = [DO]
new_data_DOsat<-merge(new_data, DOsat[,c('id','Result')], by="id")
new_data_DOsat<-plyr::rename(new_data_DOsat, c("Result.y" = "Result_DOsat", "Result.x" = "Result_DOconc"))
#merge new_data with new_data_DOsat
new_data_all<-full_join(new_data, new_data_DOsat[,c('id', 'Result_DOsat')], by="id")
#Add columns to identify exceedances of WQS for [DO] and %DO
new_data_all$Result<-as.numeric(new_data_all$Result)
new_data_all$Result_DOsat<-as.numeric(new_data_all$Result_DOsat)
# new_data_all$Cexceed <- if (new_data_all$bioc == 11) {
#   ifelse(new_data_all$Result > 11, 'Meets', 'Exceeds')
# } else {
#   ifelse(new_data_all$Result > new_data_all$bioc, 'Meets', 'Exceeds')
# }
new_data_all$Cexceed<- ifelse(new_data_all$Result > new_data_all$bioc, 'Meets', 'Exceeds')
new_data_all$Cexceed<-as.factor(new_data_all$Cexceed)
new_data_all$Sat_Exceed<-if (selectSpawning != 'No spawning') {
  ifelse(new_data_all$Result_DOsat < 95, 'Exceeds', 'Meets')
} else if (selectUseDO == 'Cold-Water Aquatic Life') {
  ifelse(new_data_all$Result_DOsat < 90, 'Exceeds', 'Meets')
} else {
  NA
}
new_data_all$Sat_Exceed<-as.factor(new_data_all$Sat_Exceed)
new_data_all$BCsat_Exceed<-ifelse(new_data_all$Cexceed == 'Exceeds' &
                                    new_data_all$Sat_Exceed == "Meets", "Meets", "Exceeds")
new_data_all$BCsat_Exceed <- as.factor(new_data_all$BCsat_Exceed)
##IF no spawning##
new_data_all$numcrit<-sdata$numcrit
new_data_all$numcrit<-as.numeric(new_data_all$numcrit)
new_data_all$Cexceed_nspwn<-ifelse(new_data_all$Result > new_data_all$numcrit, 'Meets', 'Exceeds')
new_data_all$Cexceed_nspwn<-as.factor(new_data_all$Cexceed_nspwn)
##Building the plot##
##Generate WQS Lines##
if (selectUseDO == 'Cold-Water Aquatic Life') {
  d<-data.frame(x = c(x.min, x.max), y = rep(8, 2),
                variable = rep("Cold-Water Aquatic Life", 2))
} else if (selectUseDO == 'Cool-Water Aquatic Life') {
  d<-data.frame(x = c(x.min, x.max), y = rep(6.5, 2),
                variable = rep("Cool-Water Aquatic Life", 2))
} else if (selectUseDO == 'Warm-Water Aquatic Life') {
  d<-data.frame(x = c(x.min, x.max), y = rep(5.5, 2),
                variable = rep("Warm-Water Aquatic Life", 2))
} else if (selectUseDO == 'Estuarine Water') {
  d<-data.frame(x = c(x.min, x.max), y = rep(6.5, 2),
                variable = rep("Estuarine Waters", 2))
}
##filter points that meet because of the dissolved oxygen saturation##
BCsat<-new_data_all%>%
  filter(BCsat_Exceed == "Meets", Cexceed_nspwn == "Exceeds")
BCsat_spwn<-new_data_all%>%
  filter(BCsat_Exceed == "Meets")
##PLOT THE TIMESERIES
if (selectSpawning == 'No spawning') {
  g <- ggplot(data = new_data_all, aes(x = Sampled, y = Result, color = Cexceed_nspwn)) +
    geom_point() +
    geom_point(data = BCsat, color = 'green')+
    scale_colour_manual(name = 'Key', values = c('black', 'pink')) +
    geom_hline(data = d, aes(yintercept = y), linetype = "dashed", color = "red") +
    ggtitle(bquote(atop(.(title)))) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.direction = 'horizontal') +
    xlab(x.lab) +
    ylab(y.lab) +
    xlim(x.lim) +
    ylim(y.lim)
  g
} else {
  g <- ggplot(data = new_data_all, aes(x = Sampled, y = Result, color = Cexceed)) +
    geom_point() +
    geom_point(data = BCsat_spwn, color = 'green')+
    scale_colour_manual(name = 'Key', values = c('pink', 'black')) +
    geom_hline(data = d, aes(yintercept = y), linetype = "dashed", color = "red") +
    ggtitle(bquote(atop(.(title)))) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.direction = 'horizontal') +
    xlab(x.lab) +
    ylab(y.lab) +
    xlim(x.lim) +
    ylim(y.lim)
  g
}