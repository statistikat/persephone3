#' @description create a new single object
#' @param x an object of class [persephone].
#' @param timespan number of months from the end of the time series
#' where outliers are not fixed
#' @param verbose if TRUE the changed parameters will be reported
fixOutlier = function(x, timespan = 12, verbose = FALSE) {
  # xxx <<- x
  if(is.null(x$output)){
    warning("not run yet.")
    return(invisible(NULL))
  }
  from <- time(x$ts)[length(x$ts)-timespan+1]
  y <- floor(from)
  m <- as.character(round((from-floor(from))*x$tsp[3]+1))
  if(frequency(x$ts)==4){
    m <- c("01","04","07","10")[as.numeric(m)]
  }
  if(nchar(m)==1){
    m <- paste0("0",m)
  }
  from <- paste0(y,"-",m,"-01")
  if(x$output$regarima$specification$outliers$enabled){
    possibleOutliers <- row.names(x$output$regarima$regression.coefficients)
    possibleOutliers <- possibleOutliers[substring(possibleOutliers,1,2)%in%
                                           c("AO","LS","TC")]
    if(x$tsp[3]==12){
      outliers <- lapply(possibleOutliers, function(x){
        x2 <- strsplit(x = substring(x,5,nchar(x)-1), split ="-")[[1]]
        x2[1] <- ifelse(nchar(x2[1])==1,paste0("0",x2[1]),x2[1])
        data.frame(type=substr(x,1,2),date=paste0(x2[2],"-",x2[1],"-01"))
      })
    }else if(x$tsp[3]==4){
      outliers <- lapply(possibleOutliers, function(x){
        x2 <- strsplit(x = substring(x,5,nchar(x)-1), split ="-")[[1]]
        x2[1] <- c(I="01",II="04",III="07",IV="10")[x2[1]]
        data.frame(type=substr(x,1,2),date=paste0(x2[2],"-",x2[1],"-01"))
      })
    }
    if(!is.na(x$output$regarima$specification$regression$userdef$outliers[1])){
      if(x$tsp[3]==12){
        outliers <- outliers[sapply(outliers,
                                    userdefOut = x$output$regarima$specification$regression$userdef$outliers[,1:2],
                                    function(x,userdefOut){
                                      m <- merge(x, userdefOut, by = c("type","date"))
                                      return(nrow(m)==0)
                                    })]

      }else if(x$tsp[3]==4){
        outliers <- outliers[sapply(outliers,
                                    userdefOut = x$output$regarima$specification$regression$userdef$outliers[,1:2],
                                    function(x,userdefOut){
                                      m <- merge(x, userdefOut, by = c("type","date"))
                                      return(nrow(m)==0)
                                    })]
      }
      oldType <- x$output$regarima$specification$regression$userdef$outliers$type
      oldDate <- x$output$regarima$specification$regression$userdef$outliers$date
    }else{
      oldType <- NULL
      oldDate <- NULL
    }
    if(length(outliers)>0){
      if(verbose){
        for(i in seq_along(outliers)){
          message(outliers[[i]]$type," outlier saved at ",outliers[[i]]$date,".")
        }
      }
      df <- data.frame(type=c(oldType,
                              sapply(outliers, function(x)x$type)),
                       date=c(oldDate,
                              sapply(outliers, function(x)x$date)))
      df <- unique(df)
      if(class(x)[1]=="hierarchicalTimeSeries"){
        private$updateParamsDirect(usrdef.outliersEnabled = TRUE,
                                   usrdef.outliersType = df$type,
                                   usrdef.outliersDate = df$date)
      }else{
        x$updateParams(usrdef.outliersEnabled = TRUE,
                       usrdef.outliersType = df$type,
                       usrdef.outliersDate = df$date)
      }

    }else{
      if(verbose){
        message("No automatic outliers found.")
      }
    }
    if(verbose){
      message("Updating parameter outlier.from to '",from,"'")
    }
    if(class(x)[1]=="hierarchicalTimeSeries"){
      private$updateParamsDirect(outlier.from = from)
    }else{
      x$updateParams(outlier.from = from)
    }

  }else if(verbose){
    message("Automatic outliers not enabled.")
  }
}

# noch an rjd3 anpassen
