plot.ph <- function(new_data, 
                    sea_ken_table,
                    ph_crit,
                    analyte_column = 'Analyte',
                    result_column = 'Result',
                    station_id_column = 'Station_ID',
                    station_desc_column = 'Station_Description',
                    datetime_column = 'Sampled', 
                    datetime_format = '%Y-%m-%d %H:%M:%S', 
                    plot_trend = FALSE,
                    plot_criteria,
                    plan_area) {
  require(ggplot2)
  new_data$Sampled <- as.POSIXct(strptime(new_data[, datetime_column], 
                                          format = datetime_format))  
  x.min <- as.POSIXct(strptime(min(new_data$Sampled), format = datetime_format))
  x.max <- as.POSIXct(strptime(max(new_data$Sampled), format = datetime_format))
  x.lim <- c(x.min, x.max) 
  y.min <- ifelse(floor(min(new_data[, result_column]))< 4,
                  floor(min(new_data[, result_column])), 4) 
  y.max <- ifelse(ceiling(max(new_data[, result_column])) > 10,
                  ceiling(max(new_data[, result_column])), 10) 
  y.lim <- c(y.min, y.max) 
  title <- paste0(min(new_data[, station_desc_column]), ", ID = ", 
                  min(new_data[, station_id_column])) 
  x.lab <- "Date"
  y.lab <- unique(new_data[, analyte_column])[1]
  ####definitions for drawing Seasonal Kendall slope line
  y.median <- median(new_data[, result_column])
  slope <- suppressWarnings(
    as.numeric(
      sea_ken_table[sea_ken_table$Station_ID == 
                      unique(new_data[, station_id_column]) & 
                      sea_ken_table$analyte == 
                      unique(new_data[, analyte_column]), 'slope']
      )
    )
  p.value <- suppressWarnings(
    as.numeric(
      sea_ken_table[sea_ken_table$Station_ID == 
                      unique(new_data[,station_id_column]) & 
                      sea_ken_table$analyte == 
                      unique(new_data[,analyte_column]),'pvalue']
      )
    )
  p.value.label <- sea_ken_table[sea_ken_table$Station_ID == 
                                   unique(new_data[,station_id_column]) & 
                                   sea_ken_table$analyte == 
                                   unique(new_data[,analyte_column]),'signif']
  x.delta <- as.numeric((x.max-x.min)/2)####average date
  SK.min <- y.median - x.delta*slope/365.25#minimum y value for line
  SK.max <- y.median + x.delta*slope/365.25#maximum y value for line
  sub.text <- paste0("p value = " ,
                     round(p.value, digits=3),
                     ", ",  
                     p.value.label, 
                     ", slope = ", 
                     round(slope, digits=2), 
                     ", n = ", 
                     nrow(new_data))
  df_trend_line <- data.frame(x = c(x.min - 10000, x.max + 10000),
                              y = c(SK.min, SK.max),
                              variable = rep('Trend line', 2))
  
  #Evaluate against standard
  new_data <- EvaluatepHWQS(new_data = new_data, 
                            ph_crit = ph_crit, 
                            PlanName = plan_area, 
                            selectpHCrit = plot_criteria)
  new_data$exceed <- factor(new_data$exceed, levels = c(0, 1), 
                            labels = c('Meets', 'Exceeds'))
  
  #Extract basin specific ph criteria
  OWRD_basin <- strsplit(plot_criteria, " - ")[[1]][1]
  crit_selected <- strsplit(plot_criteria, " - ")[[1]][2]
  ph_crit_min <- ph_crit[ph_crit$ph_standard == crit_selected &
                           ph_crit$OWRD_basin == OWRD_basin &
                           ph_crit$plan_name == plan_area, 'ph_low']
  ph_crit_max <- ph_crit[ph_crit$ph_standard == crit_selected &
                           ph_crit$OWRD_basin == OWRD_basin &
                           ph_crit$plan_name == plan_area, 'ph_high']

  df_ph_crit_max <- data.frame(x = c(x.min - 10000, x.max + 10000),
                               y = rep(ph_crit_max, 2),
                               variable = rep('pH Criteria', 2))
  df_ph_crit_min <- data.frame(x = c(x.min - 10000, x.max + 10000),
                               y = rep(ph_crit_min, 2),
                               variable = rep('pH Criteria', 2))
  
    ####plot the timeseries
  g <- ggplot(data = new_data, aes_string(x = 'Sampled', y = result_column, colour = 'exceed')) + 
    geom_point() + 
    ggtitle(bquote(atop(.(title), atop(paste(.(sub.text)))))) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.direction = 'horizontal') +
    xlab(x.lab) + 
    ylab(y.lab) + 
    xlim(x.lim) +
    ylim(y.lim) 
  if (plot_trend & !is.na(p.value)) {
    g <- g + geom_line(aes(x = x, y = y, color = variable), data = df_trend_line)  
  }
  g <- g + geom_line(aes(x = x, y = y, color = variable), data = df_ph_crit_min, linetype = 'dashed')
  g <- g + geom_line(aes(x = x, y = y, color = variable), data = df_ph_crit_max, linetype = 'dashed')
  if (plot_trend & !is.na(p.value)) {
    if ('Exceeds' %in% unique(new_data$exceed)) {
      g <- g + scale_color_manual("", values = c('red', 'black', 'black', 'blue'),
                                  guide = guide_legend(override.aes = list(
                                    linetype = c("blank", "blank", "dashed", "solid"),
                                    shape = c(19, 19, NA, NA))))
    } else {
      g <- g + scale_color_manual("", values = c('black', 'black', 'blue'),
                                  guide = guide_legend(override.aes = list(
                                    linetype = c("blank","dashed", "solid"),
                                    shape = c(19, NA, NA))))
    }
  } else {
    if ('Exceeds' %in% unique(new_data$exceed)) {
      g <- g + scale_color_manual("", values = c('red', 'black', 'black'),
                                  guide = guide_legend(override.aes = list(
                                    linetype = c("blank", "blank", "dashed"),
                                    shape = c(19, 19, NA))))
    } else {
      g <- g + scale_color_manual("", values = c('black', 'black'),
                                  guide = guide_legend(override.aes = list(
                                    linetype = c("blank", "dashed"),
                                    shape = c(19,  NA))))
    }
  }
  g  
}

plot.Temperature <- function(new_data, 
                             all_data,
                             selectUse,
                             selectSpawning,
                             station_id_column = 'Station_ID',
                             station_desc_column = 'Station_Description',
                             datetime_column = 'date', 
                             datetime_format = '%Y-%m-%d', 
                             plot_trend = FALSE) {
  require(ggplot2)
  new_data <- new_data <- EvaluateTempWQS(new_data, selectUse, selectSpawning, "Station_ID") 
  new_data$Sampled <- as.POSIXct(strptime(new_data[,datetime_column], 
                                          format = datetime_format))  
  new_data$exceed <- factor(new_data$exceed, levels = c(TRUE, FALSE), labels = c('Exceeds', 'Meets'))
  x.min <- min(new_data$Sampled) 
  x.max <- max(new_data$Sampled) 
  x.lim <- c(x.min, x.max) 
  y.min <- if(floor(min(new_data$sdadm, na.rm = TRUE))<=10 ){ 
    floor(min(new_data$sdadm, na.rm = TRUE)) 
  }else{
    10
  }
  y.max <- ceiling(max(new_data$sdadm, na.rm = TRUE)) 
  y.lim <- c(y.min,y.max) 
  title <- paste0(unique(all_data[all_data[,station_id_column] == 
                                    new_data[1, station_id_column],station_desc_column]), 
                  ", ID = ", 
                  new_data[1, station_id_column])
  x.lab <- "Date"
  y.lab <- "Temperature (7DADM)"
  
  ####plot the timeseries
  g <- ggplot(data = new_data, aes(x = Sampled, y = sdadm, color = exceed)) + 
    geom_point() + 
    scale_colour_manual("",
                        values = c('red', 'black'), 
                        labels = levels(new_data$exceed)) + 
    xlab(x.lab) + 
    ylab(y.lab) + 
    xlim(x.lim) +
    ylim(y.lim) +
    ggtitle(title)
  g <- g + theme(legend.position = "top",
                 legend.title = element_blank(),
                 legend.direction = 'horizontal')
  
  ####Draw WQS 
  spn_index <- which(new_data$criteria_value == 13)
  spn_diff <- diff(spn_index)
  
  if (all(spn_diff == 1)) {
    if (length(spn_index) > 0) {
      spn_1 <- max(spn_index)
      
      if (spn_1 == nrow(new_data)) {
        #Plot non-spawn time-period
        df <- data.frame(x1 = new_data[1, 'Sampled'], 
                         x2 = new_data[spn_index[1] - 1, 'Sampled'],
                         y1 = unique(new_data[1:(spn_index[1] - 1), 
                                              'criteria_value']),
                         y2 = unique(new_data[1:(spn_index[1] - 1), 
                                              'criteria_value']))
        g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Non-spawning'),
                              data = df, inherit.aes = FALSE)
      } else {
        #Plot non-spawn time-period
        df <- data.frame(x1 = new_data[spn_1 + 1, 'Sampled'],
                         x2 = new_data[nrow(new_data), 'Sampled'],
                         y1 = unique(new_data[(spn_1 + 1):nrow(new_data), 
                                              'criteria_value']),
                         y2 = unique(new_data[(spn_1 + 1):nrow(new_data),
                                              'criteria_value']))
        g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Non-spawning'),
                              data = df, inherit.aes = FALSE)
      }
      #Plot spawn time period
      df <- data.frame(x1 = new_data[spn_index[1],'Sampled'],
                       x2 = new_data[spn_1,'Sampled'],
                       y1 = unique(new_data[spn_index[1]:spn_1,
                                            'criteria_value']),
                       y2 = unique(new_data[spn_index[1]:spn_1,
                                            'criteria_value']))
      g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Spawning'),
                            data = df, inherit.aes = FALSE)
    } else {
      df <- data.frame(x1 = new_data[1,'Sampled'],
                       x2 = new_data[nrow(new_data), 'Sampled'],
                       y1 = unique(new_data[1:nrow(new_data), 'criteria_value']),
                       y2 = unique(new_data[1:nrow(new_data), 'criteria_value']))
      g <- g + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = "Non-spawning"),
                            data = df, inherit.aes = FALSE) 
    }
  } else {
    spn_stop <- spn_index[which(spn_diff > 1)]
    spn_start <- spn_index[which(spn_diff > 1) + 1]
    nspn_start <- spn_stop + 1
    nspn_stop <- spn_start - 1
    
    for (i in 1:length(spn_start)) {
      if (i < length(spn_start)) {
        #Plot next spawn time period
        df <- data.frame(x1 = new_data[spn_start[i], 'Sampled'],
                         x2 = new_data[spn_stop[i + 1], 'Sampled'],
                         y1 = unique(new_data[spn_start[i]:spn_stop[i + 1],
                                              'criteria_value']),
                         y2 = unique(new_data[spn_start[i]:spn_stop[i + 1],
                                              'criteria_value']))
        g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Spawning'),
                              data = df, inherit.aes = FALSE)
        #Plot non-spawn time period
        df <- data.frame(x1 = new_data[nspn_start[i], 'Sampled'],
                         x2 = new_data[nspn_stop[i], 'Sampled'],
                         y1 = unique(new_data[nspn_start[i]:nspn_stop[i],
                                              'criteria_value']),
                         y2 = unique(new_data[nspn_start[i]:nspn_stop[i],
                                              'criteria_value']))
        g <- g + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = 'Non-spawning'),
                              data = df, inherit.aes = FALSE)
      } else {
        #Plot last spawn-time period
        df <- data.frame(x1 = new_data[spn_start[i], 'Sampled'],
                         x2 = new_data[max(spn_index), 'Sampled'],
                         y1 = unique(new_data[spn_start[i]:max(spn_index),
                                              'criteria_value']),
                         y2 = unique(new_data[spn_start[i]:max(spn_index),
                                              'criteria_value']))
        g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Spawning'),
                              data = df, inherit.aes = FALSE)
        #Plot non-spawn time period
        df <- data.frame(x1 = new_data[nspn_start[i], 'Sampled'],
                         x2 = new_data[nspn_stop[i], 'Sampled'],
                         y1 = unique(new_data[nspn_start[i]:nspn_stop[i],
                                              'criteria_value']),
                         y2 = unique(new_data[nspn_start[i]:nspn_stop[i],
                                              'criteria_value']))
        g <- g + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = 'Non-spawning'),
                              data = df, inherit.aes = FALSE)
        #Plot last non-spawn time period
        if (new_data[nrow(new_data),'criteria_value'] != 13) {
          df <- data.frame(x1 = new_data[max(spn_index) + 1, 'Sampled'],
                           x2 = new_data[nrow(new_data), 'Sampled'],
                           y1 = unique(new_data[(max(spn_index) + 1):nrow(new_data),
                                                'criteria_value']),
                           y2 = unique(new_data[(max(spn_index) + 1):nrow(new_data),
                                                'criteria_value']))
          g <- g + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = 'Non-spawning'),
                                data = df, inherit.aes = FALSE)
        }
      }
    }
    
    #Plot first non-spawn time period TODO: Add functionality to check if start of data is in spawning or non-spawning
    if (spn_index[1] != 1) {
      df <- data.frame(x1 = new_data[1, 'Sampled'],
                       x2 = new_data[spn_index[1] - 1, 'Sampled'],
                       y1 = unique(new_data[spn_index[1]:spn_stop[1],
                                            'criteria_value']),
                       y2 = unique(new_data[spn_index[1]:spn_stop[1],
                                            'criteria_value']))
      g <- g + geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = 'Non-spawning'),
                            data = df, inherit.aes = FALSE)
    }
    
    #Plot first spawn time period
    df <- data.frame(x1 = new_data[spn_index[1],'Sampled'],
                     x2 = new_data[spn_stop[1],'Sampled'],
                     y1 = unique(new_data[spn_index[1]:spn_stop[1],
                                          'criteria_value']),
                     y2 = unique(new_data[spn_index[1]:spn_stop[1],
                                          'criteria_value']))
    g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Spawning'),
                          data = df, inherit.aes = FALSE)
  }
  
  g <- g + scale_linetype_manual(values = c('Non-spawning' = 5,
                                            'Spawning' = 2))
  g <- g + theme(legend.position = "top",
                 legend.title = element_blank(),
                 legend.direction = 'horizontal')
  
  g
}


plot.ecoli <- function(new_data, 
                       sea_ken_table,
                       analyte_column = 'Analyte',
                       result_column = 'Result',
                       station_id_column = 'Station_ID',
                       station_desc_column = 'Station_Description',
                       datetime_column = 'Sampled',
                       datetime_format = '%Y-%m-%d',
                       plot_trend = FALSE,
                       plot_log = FALSE,
                       x_min = min(new_data$Sampled),
                       x_max = max(new_data$Sampled)) {
  x.min <- as.POSIXct(strptime(x_min, format = '%Y-%m-%d'))
  x.max <- as.POSIXct(strptime(x_max, format = '%Y-%m-%d'))
  x.lim <- c(x.min, x.max) 
  y.min <- if(floor(min(new_data[,result_column]))<=0 & plot_log){
    1 
  }else{
    floor(min(new_data[,result_column]))
  }
  y.max <- max(ceiling(max(new_data[,result_column])),415) 
  y.lim <- c(y.min,y.max) 
  title <- paste0(min(new_data[,station_desc_column]), ", ID = ", 
                  min(new_data[,station_id_column]))
  x.lab <- "Date"
  y.lab <- "E. Coli"
  ####definitions for drawing Seasonal Kendall slope line
  y.median <- median(new_data[,result_column])
  x.median <- ifelse(any(new_data[,result_column] ==  y.median),
                     as.numeric(new_data[which(new_data[,result_column] == 
                                                 y.median),
                                         datetime_column])[1],
                     as.numeric(new_data[which.min(abs(
                       new_data[,result_column] - y.median)),
                       datetime_column])[1])
  slope <- as.numeric(sea_ken_table[sea_ken_table$Station_ID == 
                               unique(new_data[,station_id_column]) & 
                               sea_ken_table$analyte == 
                               unique(new_data[,analyte_column]),'slope'])
  p.value <- as.numeric(sea_ken_table[sea_ken_table$Station_ID== 
                                 unique(new_data[,station_id_column]) & 
                                 sea_ken_table$analyte == 
                                 unique(new_data[,analyte_column]),'pvalue'] )
  p.value.label <- sea_ken_table[sea_ken_table$Station_ID == 
                            unique(new_data[,station_id_column]) & 
                            sea_ken_table$analyte == 
                            unique(new_data[,analyte_column]),'signif'] 
  x.delta <- as.numeric((x.max-x.min)/2)####average date
  SK.min <- y.median-x.delta*slope/365.25#minimum y value for line
  SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
  sub.text <- paste0("p value = " ,
                     round(p.value, digits=3),
                     ", ",  
                     p.value.label, 
                     ", slope = ", 
                     round(slope, digits=2), 
                     ", n = ", 
                     nrow(new_data))
  if (!is.na(SK.min) & SK.min < 0) {
    b <- SK.min - slope*as.numeric(x.min)
    SKx.min <- as.POSIXct((1 - b)/slope, origin = "1970-01-01")
    df_trend_line <- data.frame(x = c(SKx.min, x.max),
                                y = c(1, SK.max),
                                variable = rep('Trend line', 2))
  } else if (!is.na(SK.max) & SK.max < 0) {
    b <- SK.max - slope*as.numeric(x.max)
    SKx.max <- as.POSIXct((1 - b)/slope, origin = "1970-01-01")
    df_trend_line <- data.frame(x = c(x.min, SKx.max),
                                y = c(SK.min, 1),
                                variable = rep('Trend line', 2))
  } else {
    df_trend_line <- data.frame(x = c(x.min, x.max),
                                y = c(SK.min, SK.max),
                                variable = rep('Trend line', 2))
  }
  
  #Evalute the WQS
  new_data <- EvaluateEColiWQS(new_data)
  gm_table <- attr(new_data, 'ecoli_gm_eval')
  new_data$exceed <- factor(new_data$exceed, levels = c(0, 1), 
                            labels = c('Meets', 'Exceeds'))
  gm_table$exceed <- factor(gm_table$exceed, levels = c(0, 1), 
                            labels = c('Meets', 'Exceeds'))
  gm_table$Sampled <- as.POSIXct(strptime(gm_table$day, format = "%Y-%m-%d"))
  
  #Define lines for WQS
  df_gm <- data.frame(x = c(x.min, x.max), y = rep(126, 2), 
                      variable = rep('Geometric mean WQS', 2))
  df_ss <- data.frame(x = c(x.min, x.max), y = rep(406, 2),
                      variable = rep('Single sample WQS', 2))

  ####plot the timeseries
  if (nrow(gm_table) > 0) {
    gm_table <- plyr::rename(gm_table, c('gm' = 'Result'))
    gm_table$Type <- 'Geometric mean'
    new_data$Type <- 'Single sample'
    plot_data <- rbind(new_data[,c('Sampled','Result','Type','exceed')], 
                       gm_table[,c('Sampled','Result','Type','exceed')])
  } else {
    plot_data <- new_data
  }
  plot_data$exceed_type <- paste(plot_data$exceed, plot_data$Type)
  
  g <- ggplot(data = plot_data, aes_string(x = 'Sampled', y = result_column, 
                                           colour = 'exceed_type', 
                                           shape = 'exceed_type')) + 
    geom_point() + 
    ggtitle(bquote(atop(.(title), atop(paste(.(sub.text)))))) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          legend.direction = 'horizontal') +
    xlab(x.lab) + 
    ylab(y.lab) + 
    xlim(x.lim) +
    ylim(y.lim) 
  
  if (plot_trend & !is.na(p.value)) {
    g <- g + geom_line(aes(x = x, y = y, color = variable, shape = ''), 
                       data = df_trend_line)  
  }
  
  g <- g + geom_line(aes(x = x, y = y, color = variable, shape = ''), 
                     data = df_ss, linetype = 'dashed')
  
  if (nrow(gm_table) > 0) {
    g <- g + geom_line(aes(x = x, y = y, color = variable, shape = ''), 
                       data = df_gm, linetype = 'dotdash')
    
    if (plot_trend & !is.na(p.value)) {
      if ('Exceeds' %in% unique(plot_data$exceed)) {
        if (all(c('Exceeds Geometric mean','Exceeds Single sample') %in% 
                unique(plot_data$exceed_type))) {
          g <- g + scale_color_manual("", values = c('red', 'red', 'black', 
                                                     'black', 'black', 
                                                     'black', 'blue'),
                                      labels = c('Exceeds Single Sample', 
                                                 'Exceeds Geometric Mean', 
                                                 'Geomteric Mean WQS', 
                                                 'Meets Geometric Mean', 
                                                 'Meets Single Sample',
                                                 'Single Sample WQS',
                                                 'Trend line'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c("blank", "blank", "dotdash", 
                                                     "blank","blank", 
                                                     "dashed", "solid"),
                                        shape = c(19, 17, NA, 17, 19, NA, NA)),
                                        nrow = 2))
          g <- g + scale_shape_manual("", values = c(NA, 17, 19, 17, 19),
                                      guide = FALSE)
        } else if (!'Exceeds Geometric mean' %in% unique(plot_data$exceed_type)) {
          g <- g + scale_color_manual("", values = c('red', 'black', 
                                                     'black', 'black', 
                                                     'black', 'blue'),
                                      labels = c('Exceeds Single Sample', 
                                                 'Geomteric Mean WQS', 
                                                 'Meets Geometric Mean', 
                                                 'Meets Single Sample',
                                                 'Single Sample WQS',
                                                 'Trend line'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c("blank", "dotdash", 
                                                     "blank","blank", 
                                                     "dashed", "solid"),
                                        shape = c(19, NA, 17, 19, NA, NA)),
                                        nrow = 2))
          g <- g + scale_shape_manual("", values = c(NA, 19, 17, 19),
                                      guide = FALSE)
        } else {
          g <- g + scale_color_manual("", values = c('red', 'black', 
                                                     'black', 'black', 
                                                     'black', 'blue'),
                                      labels = c('Exceeds Geometric Mean', 
                                                 'Geomteric Mean WQS', 
                                                 'Meets Geometric Mean', 
                                                 'Meets Single Sample',
                                                 'Single Sample WQS',
                                                 'Trend line'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c("blank", "dotdash", 
                                                     "blank","blank", 
                                                     "dashed", "solid"),
                                        shape = c(17, NA, 17, 19, NA, NA)),
                                        nrow = 2))
          g <- g + scale_shape_manual("", values = c(NA, 17, 17, 19),
                                      guide = FALSE)
        }
      } else {
        g <- g + scale_color_manual("", values = c('black', 
                                                   'black', 'black', 
                                                   'black', 'blue'),
                                    labels = c('Geomteric Mean WQS', 
                                               'Meets Geometric Mean', 
                                               'Meets Single Sample',
                                               'Single Sample WQS',
                                               'Trend line'),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("dotdash", 
                                                   "blank","blank", 
                                                   "dashed", "solid"),
                                      shape = c(NA, 17, 19, NA, NA)),
                                      nrow = 2))
        g <- g + scale_shape_manual("", values = c(NA, 17, 19, 17, 19),
                                    guide = FALSE)
      }
    } else {
      if ('Exceeds' %in% unique(plot_data$exceed)) {
        if (all(c('Exceeds Geometric mean','Exceeds Single sample') %in% 
                unique(plot_data$exceed_type))) {
          g <- g + scale_color_manual("", values = c('red', 'red', 'black', 
                                                     'black', 'black', 'black'),
                                      labels = c('Exceeds Single Sample', 
                                                 'Exceeds Geometric Mean', 
                                                 'Geomteric Mean WQS', 
                                                 'Meets Geometric Mean', 
                                                 'Meets Single Sample',
                                                 'Single Sample WQS'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c("blank", "blank", "dotdash", 
                                                     "blank","blank", "dashed"),
                                        shape = c(19, 17, NA, 17, 19, NA)),
                                        nrow = 2))
          g <- g + scale_shape_manual("", values = c(NA, 17, 19, 17, 19),
                                      guide = FALSE)
        } else if (!'Exceeds Geometric mean' %in% unique(plot_data$exceed_type)) {
          g <- g + scale_color_manual("", values = c('red', 'black', 
                                                     'black', 'black', 'black'),
                                      labels = c('Exceeds Single Sample',  
                                                 'Geomteric Mean WQS', 
                                                 'Meets Geometric Mean', 
                                                 'Meets Single Sample',
                                                 'Single Sample WQS'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c("blank", "dotdash", 
                                                     "blank","blank", "dashed"),
                                        shape = c(19, NA, 17, 19, NA)),
                                        nrow = 2))
          g <- g + scale_shape_manual("", values = c(NA, 19, 17, 19),
                                      guide = FALSE)
        } else {
          g <- g + scale_color_manual("", values = c('red', 'black', 
                                                     'black', 'black', 'black'),
                                      labels = c('Exceeds Geometric Mean', 
                                                 'Geomteric Mean WQS', 
                                                 'Meets Geometric Mean', 
                                                 'Meets Single Sample',
                                                 'Single Sample WQS'),
                                      guide = guide_legend(override.aes = list(
                                        linetype = c("blank", "dotdash", 
                                                     "blank","blank", "dashed"),
                                        shape = c(17, NA, 17, 19, NA)),
                                        nrow = 2))
          g <- g + scale_shape_manual("", values = c(NA, 17, 17, 19),
                                      guide = FALSE)
        }
        
      } else {
        g <- g + scale_color_manual("", values = c('black', 'black', 
                                                   'black', 'black'),
                                    labels = c('Geomteric Mean WQS', 
                                               'Meets Geometric Mean', 
                                               'Meets Single Sample',
                                               'Single Sample WQS'),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("dotdash", "blank", 
                                                   "blank", "dashed"),
                                      shape = c(NA, 17, 19, NA)),
                                      nrow = 2))
        g <- g + scale_shape_manual("", values = c(NA, 17, 19),
                                    guide = FALSE)
      }
    }
  } else {
    if (plot_trend & !is.na(p.value)) {
      if ('Exceeds' %in% unique(plot_data$exceed)) {
        g <- g + scale_color_manual("", values = c('red', 'black', 
                                                   'black', 'blue'),
                                    labels = c('Exceeds Single Sample', 
                                               'Meets Single Sample',
                                               'Single Sample WQS',
                                               'Trend line'),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("blank", "blank", 
                                                   "dashed", "solid"),
                                      shape = c(19, 19, NA, NA)),
                                      nrow = 2))
        g <- g + scale_shape_manual("", values = c(NA, 19, 19),
                                    guide = FALSE)
      } else {
        g <- g + scale_color_manual("", values = c('black', 'black', 'blue'),
                                    labels = c('Meets Single Sample',
                                               'Single Sample WQS',
                                               'Trend line'),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("blank", "dashed", "solid"),
                                      shape = c(19, NA, NA)),
                                      nrow = 2))
        g <- g + scale_shape_manual("", values = c(NA, 19),
                                    guide = FALSE)
      }
    } else {
      if ('Exceeds' %in% unique(plot_data$exceed)) {
        g <- g + scale_color_manual("", values = c('red', 'black', 'black'),
                                    labels = c('Exceeds Single Sample', 
                                               'Meets Single Sample',
                                               'Single Sample WQS'),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("blank", "blank", "dashed"),
                                      shape = c(19, 19, NA)),
                                      nrow = 2))
        g <- g + scale_shape_manual("", values = c(NA, 19, 19),
                                    guide = FALSE)
      } else {
        g <- g + scale_color_manual("", values = c('black', 'black'),
                                    labels = c('Meets Single Sample',
                                               'Single Sample WQS'),
                                    guide = guide_legend(override.aes = list(
                                      linetype = c("blank", "dashed"),
                                      shape = c(19, NA)),
                                      nrow = 2))
        g <- g + scale_shape_manual("", values = c(NA, 19),
                                    guide = FALSE)
      }
    }
  }

  g  
}

plot.entero <- function (new_data, 
                         sea_ken_table,
                         gm_table,
                         analyte_column = 'Analyte',
                         result_column = 'Result',
                         station_id_column = 'Station_ID',
                         station_desc_column = 'Station_Description',
                         datetime_column = 'Sampled', 
                         datetime_format = '%Y-%m-%d', 
                         plot_trend = FALSE,
                         plot_log = FALSE,
                         x_min = min(new_data$Sampled),
                         x_max = max(new_data$Sampled)) {
  x.min <- as.POSIXct(strptime(x_min, format = '%Y-%m-%d'))#min(new_data$Sampled) #min of subset date
  x.max <- as.POSIXct(strptime(x_max, format = '%Y-%m-%d'))#max(new_data$Sampled) #max of subset date
  x.lim <- c(x.min, x.max) ####define the data domain for graph
  y.min <- if(floor(min(new_data[,result_column]))<=0 & plot_log){ #min of data for graph
    1 #set minimum y value for log scale to one
  }else{
    floor(min(new_data[,result_column]))
  }
  y.max <- max(ceiling(max(new_data[,result_column])),415) #max of data for graph
  y.lim <- c(y.min,y.max) ####define the data range
  title <- paste0(min(new_data[,station_desc_column]), 
                  ", ID = ", 
                  min(new_data[,station_id_column]))
  x.lab <- "month"
  y.lab <- "Enterococcus"
  ####definitions for drawing Seasonal Kendall slope line
  y.median <- median(new_data[,result_column])
  x.median <- as.numeric(new_data[which(new_data[,result_column] == 
                                          floor(y.median)),datetime_column])[1]
  slope <- as.numeric(sea_ken_table[sea_ken_table$Station_ID == 
                               unique(new_data[,station_id_column]) & 
                               sea_ken_table$analyte == 
                               unique(new_data[,analyte_column]),'slope'])
  p.value <- as.numeric(sea_ken_table[sea_ken_table$Station_ID== 
                                 unique(new_data[,station_id_column]) & 
                                 sea_ken_table$analyte == 
                                 unique(new_data[,analyte_column]),'pvalue'] )
  p.value.label <- sea_ken_table[sea_ken_table$Station_ID == 
                            unique(new_data[,station_id_column]) & 
                            sea_ken_table$analyte == 
                            unique(new_data[,analyte_column]),'signif'] 
  b_med = y.median - (slope/365.25/24/60/60)*x.median
  trend_fun <- function (x, b = b_med, m = slope) 
    {(m/365.25/24/60/60)*(as.numeric(x)) + b}
  x.delta <- as.numeric((x.max-x.min)/2)####average date
  SK.min <- y.median-x.delta*slope/365.25#minimum y value for line
  SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
  sub.text <- paste0("p value = " ,
                     round(p.value, digits=3),
                     ", ",  
                     p.value.label, 
                     ", slope = ", 
                     round(slope, digits=2), 
                     ", n = ", 
                     nrow(new_data))
  ####plot the timeseries
  par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,3.1,2.1)) 
  plot(new_data$Sampled, new_data[,result_column], 
       xlim=x.lim, ylim=y.lim, 
       xlab="", ylab=y.lab, 
       bty="L", log = ifelse(plot_log,"y","")) ####plot the points , log=log.scale  
  points(as.POSIXct(strptime(gm_table$day, format = "%Y-%m-%d")), 
         gm_table$gm, pch = 2)
  title(main=title, cex.main=1.2, outer=TRUE)
  mtext(text=sub.text, side=3,cex=1.0, outer=TRUE)
  exceeds.points.sampled <- new_data[new_data[,result_column] > 158,]
  points(exceeds.points.sampled$Sampled, exceeds.points.sampled[,result_column], 
         col="red", pch=20) ####plot the exceedances
  if (nrow(gm_table) > 0) {
    gm_table$Sampled <- as.POSIXct(gm_table$day)
    exceeds.points.gm <- gm_table[gm_table$gm > 35,]
    points(exceeds.points.gm$Sampled, exceeds.points.gm$gm, col = "maroon", 
           pch = 17)
  }
  if (plot_trend){
    if (plot_log) {
      curve(expr = trend_fun, add = TRUE, col="red", lwd=2)
    } else {
      lines(x=c(x.min, x.max), y=c(SK.min, SK.max), col="red", lwd=2)#draw Seasonal Kendall slope line using median concentration at average date 
    }
  }
  lines(x=c(x.min, x.max), y=c(158, 158), lty=2)#draw WQS 
  lines(x=c(x.min, x.max), y=c(35, 35), lty=3)#draw WQS 
  legend(x=x.min,y=y.min, 
         legend=c("Maximum criterion", "Geomean criterion", 
                  "Seasonal Kendall trend", "Single Sample", 
                  "Geomean Values"), 
         lty=c(2,3,1,NA,NA), 
         pch=c(NA,NA,NA,1,2), 
         col=c("black","black", "red","black", "black"), 
         lwd=c(1,1,2), 
         xjust=-0.01, 
         yjust=-8, 
         box.lty=0, 
         cex=1.0, 
         horiz=TRUE)
}