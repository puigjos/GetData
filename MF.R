require(zoo)
require(readr)
data = NULL


get_MF <- function(path){
  require(zoo)
  require(data.table)
  require(readxl)
  temp <- tempfile()
  download.file(path, temp, mode = 'wb')
  data = list()
  n <- 1
  dt <- c()
  while(class(dt)[1] != 'try-error'){
    dt <-  try(data.table(read_xls(temp, sheet=n)), silent = T)
    if(class(dt)[1] != 'try-error'){
      data[[n]] = dt 
    }
    n <- n+1
  }
  unlink(temp) 
  limpieza <- function(dt){
    m <- min(which(!is.na(dt[[2]])))
    year <- readr::parse_number(as.character(unlist(dt[m,])))
    year[1] <- 'Variable'
    r = NULL
    for(i in 2:length(year)){
      if(is.na(year[i])){
        year[i] = v
      }else{
        v = year[i]
      }
    }
    
    trimester <- readr::parse_number(as.character(unlist(dt[(m+2),])))
    trimester[1] <- 'Variable'
    r = NULL
    for(i in 2:length(year)){
      if(is.na(year[i])){
        year[i] = v
      }else{
        v = year[i]
      }
    }
    YQ = c('Variable', as.character(as.yearqtr(paste0(year[-1],'-', trimester[-1]))))
    
    dt1 <- dt[!1:7,]
    colnames(dt1) = YQ
    dt2 <- data.table(names(dt1)[-1],transpose(dt1[,-1]))
    colnames(dt2) = c('Date', dt1[[1]])
    dt2 <- as.data.table(dt2)
    dt2[, names(dt2)[-1] := lapply(.SD, function(x){as.numeric(as.character(x))}),
                                   .SD = names(dt2)[-1]]
    
    return(dt2)
  }
  
  dts <- rbindlist(lapply(data, limpieza),fill = T)
  dts <- dts[1:(nrow(dts) - 2)]
  dts$Date <- as.yearqtr(dts$Date)
  return(dts)
}

dt = get_MF('http://www.fomento.gob.es/BE2/sedal/35101000.XLS')





