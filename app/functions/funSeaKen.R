run_seaKen <- function (df.all) {
  sea_ken_int <- data.frame(Station_ID=sort(unique(df.all$Station_ID)),
                            analyte="none",
                            slope="none",
                            pvalue="none",
                            median="none",
                            S="none",
                            varS="none",
                            N="none",
                            signif="none",
                            stringsAsFactors=FALSE)
  parms <- unique(df.all$Analyte)
  parms <- parms[parms != 'Temperature']
  for (p in 1:length(parms)) {
    parm <- parms[p]
    for(ii in 1:length(sea_ken_int$Station_ID)) { 
      # specifiy current Station_ID
      tmp.one.station <- sea_ken_int$Station_ID[ii]
      tmp.data.raw <- df.all[df.all$Station_ID == tmp.one.station & 
                               df.all$Analyte == parm,]
      sea_ken_int$analyte[ii] <- parm
      sea_ken_int$N[ii] <- length(tmp.data.raw$Result)
      if (!nrow(tmp.data.raw) > 1 | all(is.na(tmp.data.raw$Result))) {
        sea_ken_int$signif[ii] <- "Years<8"
        next
      } 
      # Reshape and manipulate data to convert to wqData-class
      tmp.data <- data.frame(date=tmp.data.raw$Sampled,
                             time="0000",
                             stn=as.character(tmp.one.station),
                             depth=1,
                             variable=parm,
                             value=suppressWarnings(
                               as.numeric(
                                 tmp.data.raw$Result)), 
                             stringsAsFactors=FALSE)
      
      # Construct an object of class "WqData"
      tmp.wq <- wqData(tmp.data, c(1,3,4), c(5,6), 
                       site.order = TRUE, 
                       type = "long",
                       time.format = "%Y-%m-%d %H:%M:%S")
      # Create time series from water quality data
      tmp.ts <- suppressWarnings(tsMake(tmp.wq, focus = parm, layer = c(0, 5)))
      if (length(unique(year(tmp.data$date))) < 8) {
        sea_ken_int$signif[ii] <- "Years<8"
      }
      if (!length(tmp.ts) > 2 |
          start(tmp.ts)[1] == end(tmp.ts)[1] | 
          !any(1:frequency(tmp.ts) %in% cycle(tmp.ts)) |
          frequency(tmp.ts) <= 1
          #| !all(1:12 %in% cycle(tmp.ts))
          ) next
      tmp.result <- seaKenPlus(tmp.ts)
      sea_ken_int$pvalue[ii] <- tmp.result$p.value
      sea_ken_int$slope[ii] <- tmp.result$sen.slope
      sea_ken_int$S[ii] <- tmp.result$S
      sea_ken_int$varS[ii] <- tmp.result$varS
      sea_ken_int$median[ii] <- suppressWarnings(
        median(as.numeric(tmp.data.raw$Result),na.rm = FALSE))
      
      rm(list=ls(pattern="tmp.*"))
    }
    
    ifelse(p == 1, SeaKen <- sea_ken_int, SeaKen <- rbind(SeaKen, sea_ken_int))
  }
  
  SeaKen$signif <- ifelse(SeaKen$signif=="Years<8",
                          "Need at least 8 years",
                          ifelse(SeaKen$pvalue<=0.01, 
                                 "99% Significance Level",
                                 ifelse(SeaKen$pvalue<=0.05, 
                                        "95% Significance Level",
                                        ifelse(SeaKen$pvalue<=0.1, 
                                               "90% Significance Level",
                                               ifelse(SeaKen$pvalue<=0.2, 
                                                      "80% Significance Level",
                                                      "Not Significant")))))
  
  return(SeaKen)
}

seaKenPlus <- function (x, plot = FALSE, type = c("slope", "relative"), order = FALSE, 
          pval = 0.05, mval = 0.5, pchs = c(19, 21), ...) 
{
  if (!is.numeric(x) && !is.matrix(x) && !is.data.frame(x)) 
    stop("'x' must be a vector, matrix, or data.frame")
  if (!is.null(ncol(x)) && is.null(colnames(x))) 
    colnames(x) <- paste("series_", 1:ncol(x), sep = "")
  type <- match.arg(type)
  sk <- function(x) {
    if (!is.ts(x)) 
      stop("'x' must be of class 'ts'")
    if (identical(frequency(x), 1)) 
      stop("'x' must be a seasonal time series with frequency > 1")
    fr <- frequency(x)
    xmod <- length(x)%%fr
    if (!identical(xmod, 0)) 
      x <- ts(c(x, rep(NA, fr - xmod)), start = start(x), 
              frequency = fr)
    x1 <- matrix(x, ncol = fr, byrow = TRUE)
    mk1 <- mannKen(x1)
    sen.slope <- median(mk1[, "sen.slope"], na.rm = TRUE)
    sen.slope.rel <- sen.slope/abs(median(x, na.rm = TRUE))
    S <- sum(mk1[, "S"])
    varS <- sum(mk1[, "varS"])
    Z <- (S - sign(S))/sqrt(varS)
    p.value <- 2 * pnorm(-abs(Z))
    miss <- round(sum(mk1[, "miss"] >= 0.5)/fr, 3)
    c(sen.slope = sen.slope, sen.slope.rel = sen.slope.rel, 
      p.value = p.value, miss = miss, S = S, varS = varS)
  }
  if (is.null(dim(x))) 
    return(as.list(sk(x)))
  if (ncol(x) == 1) 
    return(as.list(sk(x[, 1])))
  ans <- t(sapply(1:ncol(x), function(i) sk(x[, i])))
  rownames(ans) <- colnames(x)
  if (!plot) {
    ans
  }
  else {
    v1 <- switch(type, slope = "sen.slope", relative = "sen.slope.rel")
    if (order) 
      ans <- ans[order(ans[, v1]), ]
    pch <- ifelse(ans[, "miss"] >= mval, NA, ifelse(ans[, 
                                                        "p.value"] < pval, pchs[1], pchs[2]))
    dotchart(ans[, v1], pch = pch, ...)
  }
}