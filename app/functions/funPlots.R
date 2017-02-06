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
  x.min <- min(new_data$Sampled)
  x.max <- max(new_data$Sampled)
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
  df_trend_line <- data.frame(x = c(x.min + 10000, x.max - 10000),
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
                           (ph_crit$plan_name == plan_area | 
                              ph_crit$HUC8 == strsplit(plan_area, 
                                                       split = " - ")[[1]][1]), 
                         'ph_low']
  ph_crit_max <- ph_crit[ph_crit$ph_standard == crit_selected &
                           ph_crit$OWRD_basin == OWRD_basin &
                           (ph_crit$plan_name == plan_area | 
                              ph_crit$HUC8 == strsplit(plan_area, 
                                                       split = " - ")[[1]][1]), 
                         'ph_high']

  df_ph_crit_max <- data.frame(x = c(x.min + 10000, x.max - 10000),
                               y = rep(ph_crit_max, 2),
                               variable = rep('pH Criteria', 2))
  df_ph_crit_min <- data.frame(x = c(x.min + 10000, x.max - 10000),
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
  new_data <- EvaluateTempWQS(new_data, selectUse, selectSpawning, "Station_ID") 
  new_data$Sampled <- as.POSIXct(strptime(new_data[,datetime_column], 
                                          format = datetime_format))  
  new_data[!is.na(new_data$sdadm) & is.na(new_data$exceed), 'exceed'] <- FALSE
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
  if (y.max < 20 & selectUse %in% c('Salmon and Steelhead Migration Corridors',
                                    'Redband and Lanhontan Cutthroat Trout')) {
    y.max <- 21
  } else if (y.max < 18 & selectUse == 'Salmon and Trout Rearing and Migration') {
    y.max <- 19
  } else if (y.max < 16 & selectUse == 'Core Cold Water Habitat') {
    y.max <- 17
  } else if (y.max < 13) {
    y.max <- 14
  }
  y.lim <- c(y.min,y.max) 
  title <- paste0(unique(all_data[all_data[,station_id_column] == 
                                    new_data[1, station_id_column],station_desc_column]), 
                  ", ID = ", 
                  new_data[1, station_id_column])
  x.lab <- "Date"
  y.lab <- "Temperature (7DADM)"
  
  ####plot the timeseries
  if (selectSpawning == 'No spawning' & any(selectUse %in% 
                                            c('Cool water species', 
                                              'No Salmonid Use/Out of State'))) {
    g <- ggplot(data = new_data, aes(x = Sampled, y = sdadm), color = 'black') + 
      geom_point() + 
      xlab(x.lab) + 
      ylab(y.lab) + 
      xlim(x.lim) +
      ylim(y.lim) +
      ggtitle(title)
    g <- g + theme(legend.position = "top",
                   legend.title = element_blank(),
                   legend.direction = 'horizontal')
  } else {
    g <- ggplot(data = new_data, aes(x = Sampled, y = sdadm, color = exceed)) + 
      geom_point() + 
      xlab(x.lab) + 
      ylab(y.lab) + 
      xlim(x.lim) +
      ylim(y.lim) +
      ggtitle(title)
    if (all(new_data$exceed == 'Meets', na.rm = TRUE)) {
      g <- g + scale_colour_manual("",
                                   values = c('black'), 
                                   labels = 'Meets') 
    } else if (all(new_data$exceed == 'Exceeds', na.rm = TRUE)) {
      g <- g +  scale_colour_manual("",
                                    values = c('red'), 
                                    labels = 'Exceeds')
    } else {
      g <- g + scale_colour_manual("",
                                   values = c('red', 'black'), 
                                   labels = levels(new_data$exceed))
    }
    g <- g + theme(legend.position = "top",
                   legend.title = element_blank(),
                   legend.direction = 'horizontal')
    
    ####Draw WQS 
    if (selectSpawning != 'No spawning' & any(selectUse %in% 
                                              c('Cool water species', 
                                                'No Salmonid Use/Out of State'))) {
      spn_index <- which(new_data$criteria_value == 13)
      spn_diff <- diff(spn_index)
      
      if (all(spn_diff == 1)) {
        if (length(spn_index) > 0) {
          spn_1 <- max(spn_index)
          
          #Plot spawn time period
          df <- data.frame(x1 = new_data[spn_index[1],'Sampled'],
                           x2 = new_data[spn_1,'Sampled'],
                           y1 = unique(new_data[spn_index[1]:spn_1,
                                                'criteria_value']),
                           y2 = unique(new_data[spn_index[1]:spn_1,
                                                'criteria_value']))
          g <- g + geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, linetype = 'Spawning'),
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
            }
          }
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
    } else {
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
    }
    
    g <- g + scale_linetype_manual(values = c('Non-spawning' = 5,
                                              'Spawning' = 2))
    g <- g + theme(legend.position = "top",
                   legend.title = element_blank(),
                   legend.direction = 'horizontal')
  }
  g
}


plot.bacteria <- function(new_data, 
                          sea_ken_table,
                          analyte_column = 'Analyte',
                          result_column = 'Result',
                          station_id_column = 'Station_ID',
                          station_desc_column = 'Station_Description',
                          datetime_column = 'Sampled',
                          datetime_format = '%Y-%m-%d',
                          plot_trend = FALSE,
                          plot_log = FALSE,
                          parm) {
  x.min <- min(new_data$Sampled)
  x.max <- max(new_data$Sampled)
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
  y.lab <- parm
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
  if (!is.na(SK.min)) {
    if (SK.min < y.min) {SK.min <- y.min}
    if (SK.min > y.max) {SK.min <- y.max}
    
  }
  SK.max <- y.median+x.delta*slope/365.25#maximum y value for line
  if (!is.na(SK.max)) {
    if (SK.max < y.min) {SK.max <- y.min}
    if (SK.max > y.max) {SK.max <- y.max}
  }
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
    df_trend_line <- data.frame(x = c(x.min + 10000, x.max - 10000),
                                y = c(SK.min, SK.max),
                                variable = rep('Trend line', 2))
  }
  
  #Evalute the WQS
  if (parm == 'E. Coli') {
    new_data <- EvaluateEColiWQS(new_data)
    gm_table <- attr(new_data, 'ecoli_gm_eval')
    #Define lines for WQS
    df_gm <- data.frame(x = c(x.min, x.max), y = rep(126, 2), 
                        variable = rep('Geometric mean WQS', 2))
    df_ss <- data.frame(x = c(x.min, x.max), y = rep(406, 2),
                        variable = rep('Single sample WQS', 2))
  } else {
    new_data <- EvaluateEnteroWQS(new_data)
    gm_table <- attr(new_data, 'entero_gm_eval')
    #Define lines for WQS
    df_gm <- data.frame(x = c(x.min, x.max), y = rep(35, 2), 
                        variable = rep('Geometric mean WQS', 2))
    df_ss <- data.frame(x = c(x.min, x.max), y = rep(158, 2),
                        variable = rep('Single sample WQS', 2))
  }
  
  new_data$exceed <- factor(new_data$exceed, levels = c(0, 1), 
                            labels = c('Meets', 'Exceeds'))
  gm_table$exceed <- factor(gm_table$exceed, levels = c(0, 1), 
                            labels = c('Meets', 'Exceeds'))
  gm_table$Sampled <- as.POSIXct(strptime(gm_table$day, format = "%Y-%m-%d"))
  
  

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
    g <- g + geom_line(aes(x = x, y = y, color = variable, shape = '', group = variable), 
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
          if (!"Meets Geometric mean" %in% unique(plot_data$exceed_type)) {
            g <- g + scale_color_manual("", values = c('red', 'red', 'black', 
                                                       'black', 
                                                       'black', 'blue'),
                                        labels = c('Exceeds Single Sample', 
                                                   'Exceeds Geometric Mean', 
                                                   'Geomteric Mean WQS',  
                                                   'Meets Single Sample',
                                                   'Single Sample WQS',
                                                   'Trend line'),
                                        guide = guide_legend(override.aes = list(
                                          linetype = c("blank", "blank", "dotdash", 
                                                       "blank", 
                                                       "dashed", "solid"),
                                          shape = c(19, 17, NA, 19, NA, NA)),
                                          nrow = 2))
            g <- g + scale_shape_manual("", values = c(NA, 17, 19, 19),
                                        guide = FALSE)
          } else {
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
          }
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
          if ((!"Meets Geometric mean" %in% unique(plot_data$exceed_type))) {
            g <- g + scale_color_manual("", values = c('red', 'red', 'black', 
                                                        'black', 'black'),
                                        labels = c('Exceeds Single Sample', 
                                                   'Exceeds Geometric Mean', 
                                                   'Geomteric Mean WQS', 
                                                   'Meets Single Sample',
                                                   'Single Sample WQS'),
                                        guide = guide_legend(override.aes = list(
                                          linetype = c("blank", "blank", "dotdash", 
                                                       "blank", "dashed"),
                                          shape = c(19, 17, NA, 19, NA)),
                                          nrow = 2))
            g <- g + scale_shape_manual("", values = c(NA, 17, 19, 19),
                                        guide = FALSE)
          } else {
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
          }
          
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

# Multiple plot function
#
#  FROM R COOKBOOK http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (numPlots == 4) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(c(0,1,2,0,3,4),
                     nrow = ceiling(numPlots/cols) + 1, ncol = cols)
  } else {
    layout <- matrix(c(0,1,0,2),
                     nrow = ceiling(numPlots/cols) + 1, ncol = cols)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), 
                                               heights = unit(c(1, rep(4,ceiling(numPlots/cols))), "null"))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if (!is.null(title)) {
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)))
    }
  }
}

plot.DOsat<-function(new_data,
                  analyte_column = 'Analyte',
                  station_id_column = 'Station_ID',
                  station_desc_column = 'Station_Description',
                  datetime_column = 'Sampled',
                  result_column = 'Result',
                  datetime_format = '%Y-%m-%d %H:%M:%S',
                  parm) {
  require(ggplot2)
  
  
  new_data$Sampled <- as.POSIXct(strptime(new_data[, datetime_column],
                                          format = datetime_format))
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
  
  ##Building the plot
  g <- ggplot(data = new_data, aes(x = Sampled, y = Result)) +
    geom_point() +
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
  require(chron)
  #dataframe that assigns WQS values to Aquatic Life Uses
  #new_data<-DO
  new_data$Result <- as.numeric(new_data$Result)
  new_data$Sampled <- as.POSIXct(strptime(new_data[, datetime_column],
                                          format = datetime_format))
  #new_data$Result<-as.numeric(new_data$Result)
  x.min <- min(new_data$Sampled) 
  x.max <- max(new_data$Sampled) 
  x.lim <- c(x.min, x.max)
  
  title <- paste0(min(new_data[, station_desc_column]), ", ID = ",
                  min(new_data[, station_id_column]))
  x.lab <- "Date"
  y.lab <- "Dissolved Oxygen"
  
  
  ##Generate Exceedances of WQS##
  new_data$selectUseDO<-selectUseDO
  
  spd_list <- strsplit(selectSpawning, split = "-")
    spd_chron <- lapply(spd_list, function(x) {as.chron(x, format = "%B %d")})
    spd_months <- lapply(spd_chron, months)
    spd_days <- lapply(spd_chron, chron::days)
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
  y.min <- (min(sdata$numcrit) - 1) #unique(sdata$numcrit)[1] #floor(min(new_data[, result_column]))
  y.max <- ceiling(max(new_data[, result_column]))
  y.lim <- c(y.min, y.max)
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
    filter(Analyte == 'Dissolved oxygen saturation') %>%
    filter(Station_ID == unique(new_data$Station_ID))
  DOsat$Result <- as.numeric(DOsat$Result)
  DOsat$Sampled<-as.POSIXct(strptime(DOsat[, datetime_column],
                                     format = datetime_format))
  DOsat$id<-paste(DOsat$Station_ID, DOsat$Sampled, sep=" ")
  new_data$id<-paste(new_data$Station_ID, new_data$Sampled, sep=" ")
  #Result.y = results from %DO; Result.x = [DO]
  new_data_DOsat<-merge(new_data, DOsat[,c('id','Result')], by="id")
  new_data_DOsat<-plyr::rename(new_data_DOsat, c("Result.y" = "Result_DOsat", "Result.x" = "Result_DOconc"))
  #merge new_data with new_data_DOsat
  new_data_all<-dplyr::full_join(new_data, new_data_DOsat[,c('id', 'Result_DOsat')], by="id")
  #Add columns to identify exceedances of WQS for [DO] and %DO
  new_data_all$Result<-as.numeric(new_data_all$Result)
  new_data_all$Result_DOsat<-as.numeric(new_data_all$Result_DOsat)

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
  } else {
    d<-data.frame(x = c(x.min, x.max), y = rep(6.5, 2),
                  variable = rep("Estuarine Waters", 2))
  }
  
  ##filter points that meet because of the dissolved oxygen saturation##
  
  BCsat<-new_data_all%>%
    filter(BCsat_Exceed == "Meets", Cexceed_nspwn == "Exceeds")
  
  if (selectSpawning == 'No spawning'){
    BCsat<-new_data_all%>%
      filter(BCsat_Exceed == "Meets", Cexceed_nspwn == "Exceeds")
    BCsat_spwn_exceed<-c("BCsat_spwn_exceed")
    new_data_all[,BCsat_spwn_exceed] <- NA 
  } else if (is.data.frame(BCsat) && nrow(BCsat)>0) {
    BCsat$BCsat_spwn_exceed<-ifelse((length(BCsat$BCsat_Exceed) > 0), 'Meets b/c %Sat', NA)
    BCsat_spwn_exceed<-c("BCsat_spwn_exceed")
    new_data_all[,BCsat_spwn_exceed] <- NA 
  } else {
    BCsat_spwn<-new_data_all%>%
      filter(BCsat_Exceed == "Meets")
    if (nrow(BCsat_spwn) > 0){
    BCsat_spwn$BCsat_spwn_exceed<- ifelse(length(BCsat_spwn$BCsat_Exceed) > 0, 'Meets b/c %Sat', NA)
    BCsat_spwn_exceed<-c("BCsat_spwn_exceed")
    new_data_all[,BCsat_spwn_exceed] <- NA
    new_data_all$BCsat_spwn_exceed <- NA
    new_data_all<-rbind(new_data_all, BCsat_spwn) 
    } else{
      BCsat_spwn_exceed<-c("BCsat_spwn_exceed")
      new_data_all[,BCsat_spwn_exceed] <- NA
      new_data_all$BCsat_spwn_exceed <- NA
      new_data_all<-rbind(new_data_all, BCsat_spwn)
       }
    } 
   


 #BCsat_spwn$BCsat_spwn_exceed <- 'Meets b/c %Sat'
  ##PLOT THE TIMESERIES
  if (selectSpawning == 'No spawning') {
    new_data_all<-rbind(new_data_all, BCsat)
    new_data_all$exceed<-ifelse(new_data_all$Cexceed_nspwn == 'Meets', 
                                'Meets', 
                                ifelse(!is.na(new_data_all$BCsat_spwn_exceed), 
                                       "Meets b/c %Sat", 
                                       'Exceeds'))
    
    g <- ggplot(data = new_data_all, aes(x = Sampled, y = Result)) +
      geom_point(aes(color = exceed)) +
      #geom_point(aes(color = BCsat2))+
      scale_colour_manual(name = 'Key', 
                          breaks = c('Meets', 'Exceeds', 'Meets b/c %Sat'),
                          values = c('Meets' = 'black', 'Exceeds' = 'pink', 'Meets b/c %Sat' = 'green'))+
      xlim(x.lim) +
      ylim(y.lim) +
      geom_hline(data = d, aes(yintercept = y), color = "red") +
      theme(plot.title = element_text(vjust=1.5, face="bold", size = 8))+
      ggtitle(bquote(atop(.(title)))) +
      theme(legend.position = "top",
            legend.title = element_blank(),
            legend.direction = 'horizontal') +
      xlab(x.lab) +
      ylab(y.lab)
   g 
   
  } else if (length(new_data_all$BCsat_spwn_exceed) > 0 & selectSpawning != 'No spawning') {
    new_data_all$exceed<-ifelse(new_data_all$Cexceed == 'Meets', 
       'Meets', 
       ifelse(!is.na(new_data_all$BCsat_spwn_exceed), 
              "Meets b/c %Sat", 
              'Exceeds'))
    
    g <- ggplot(data = new_data_all, aes(x = Sampled, y = Result)) +
      geom_point(aes(color = exceed))+
      scale_colour_manual(name = 'Key', 
                          values = c('Meets' = 'black', 'Exceeds' = 'pink', 'Meets b/c %Sat' = 'green'))+
      xlim(x.lim) +
      ylim(y.lim) +
      geom_hline(data = d, aes(yintercept = y), color = "red") +
      theme(plot.title = element_text(vjust=1.5, face="bold", size = 8))+
      ggtitle(bquote(atop(.(title)))) +
      theme(legend.position = "top",
          legend.title = element_blank(),
         legend.direction = 'horizontal') +
      xlab(x.lab) +
      ylab(y.lab) 
    g
    
    } else {
      g <- ggplot(data = new_data_all, aes(x = Sampled, y = Result)) +
        geom_point(aes(color = Cexceed)) +
        scale_colour_manual(name = 'Key', 
                           breaks = c('Meets', 'Exceeds'),
                           values = c('Meets' = 'black', 'Exceeds' = 'pink'))+
        xlim(x.lim) +
        ylim(y.lim) +
        geom_hline(data = d, aes(yintercept = y),  color = "red") +
        theme(plot.title = element_text(vjust=1.5, face="bold", size = 8))+
        ggtitle(bquote(atop(.(title)))) +
        theme(legend.position = "top",
              legend.title = element_blank(),
              legend.direction = 'horizontal') +
        xlab(x.lab) +
        ylab(y.lab) 
      g
    } 
 
    ####DRAW WQS SPAWNING LINES
  if (selectSpawning != 'No spawning'){
  
  new_data_all <- new_data_all[order(new_data_all$Sampled),]
  data_years <- unique(lubridate::year(new_data_all$Sampled))
  whole_range <- seq(min(new_data_all$Sampled), max(new_data_all$Sampled), by = 'day')
  wr <- data.frame('Sampled' = whole_range, bioc = NA)
  
  if (any(new_data_all$winter)) {
    #rest of the spawning periods
    for (k in 1:length(data_years)) {
      spwn_strt_text <- paste(spd_list[[1]][1], data_years[k])
      spwn_end_text <- paste(spd_list[[1]][2], data_years[k] + 1)
      spwn_start<-as.POSIXct(strptime(spwn_strt_text, format = "%B %d %Y"))
      spwn_end<-as.POSIXct(strptime(spwn_end_text, format = "%B %d %Y"))
      wr[wr$Sampled >= spwn_start & wr$Sampled <= spwn_end, 'bioc'] <- 11
    }
    
    #first spawning period
    spwn_end_text <- paste(spd_list[[1]][2], data_years[1])
    spwn_end<-as.POSIXct(strptime(spwn_end_text, format = "%B %d %Y"))
    wr[wr$Sampled <= spwn_end, 'bioc'] <- 11
  } else {
    for (k in 1:length(data_years)) {
      spwn_strt_text <- paste(spd_list[[1]][1], data_years[k])
      spwn_end_text <- paste(spd_list[[1]][2], data_years[k])
      spwn_start<-as.POSIXct(strptime(spwn_strt_text, format = "%B %d %Y"))
      spwn_end<-as.POSIXct(strptime(spwn_end_text, format = "%B %d %Y"))
      wr[wr$Sampled >= spwn_start & wr$Sampled <= spwn_end, 'bioc'] <- 11
    }
  }
  
  g <- g + geom_line(aes(x = wr$Sampled,  y = wr$bioc, linetype = 'Spawning'),
                     data = wr, inherit.aes = FALSE, na.rm = TRUE)
  
  
  }
  g
}

#ggsave("g.png", height = 6, width = 6)

