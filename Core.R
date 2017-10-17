
path = 'http://www.cores.es/sites/default/files/archivos/estadisticas/consumos-pp.xlsx'


get_core <- function(path, hoja){
  require(zoo)
  require(lubridate)
  require(data.table)
  require(readxl)
  temp <- tempfile()
  download.file(path, temp, mode = 'wb')
  data <- data.table(read_xlsx(temp, sheet = hoja))
  
  m = which(data[[1]]=='Año')
  
  dt = data[!1:m]
  colnames(dt) = as.character(unlist(data[m,]))
  dt <- dt[!is.na(dt[[1]]) & dt[[2]] != 'total']
  dt$Año <- as.yearmon(dym(paste0(dt[[1]],'-', dt[[2]])))
  dt$Mes = NULL
  dt = as.data.table(dt)
  dt[, names(dt)[-1] := lapply(.SD, function(x){as.numeric(as.character(x))}), 
     .SD = names(dt)[-1]]
  return(dt)
}
dt <- get_core(path, 'Gasolinas')


