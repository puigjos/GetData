
get_INE <- function(){
  
  require(rvest)
  require(curl)
  require(httr)
  require(XML)
  require(xml2)
  require(data.table)
  require(dplyr)
  
  # First link --------------------------------------------------------------
  
  urls = 'http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177024&menu=resultados&idp=1254735976606'
  
  # sdata = read_html(iconv(urls,to = "UTF-8"), encoding = "latin1")
  sdata = read_html(urls)
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'dynt3/inebase')]"
                      , function(x){
                        data.frame(name = xmlValue(x), href = xmlGetAttr(x, "href"))
                      })
  x = do.call(rbind.data.frame, links)
  First = data.table(id = 1:nrow(x),x)
  
  
  
  # Second link -------------------------------------------------------------
  
  u <- 'http://www.ine.es'
  msg = as.character(paste(First[[1]],':', First[[2]]))
  print(msg)
  cat('Escriba el número')
  number = scan(n=1)
  
  url = paste0(u, First$href[number])
  
  # sdata = read_html(iconv(url,to = "UTF-8"), encoding = "latin1")
  sdata = read_html(url)
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'capsel')]"
                      , function(x){
                        data.frame(name = xmlValue(x, ignoreComments=T), href = xmlGetAttr(x, "href"))
                      })
  if(length(links)!=0){
    x = do.call(rbind.data.frame, links)
    Second = data.table(id = 1:nrow(x),x)
    su = paste0('\t', 1:40, collapse = '')
    Second$name = gsub(paste0('[\n\t1\t3',su,']') , '', Second$name)
    
    
    # Third link --------------------------------------------------------------
    
    u <- 'http://www.ine.es/dynt3/inebase/index.htm'
    msg = as.character(paste(Second[[1]],':', Second[[2]]))
    print(msg)
    cat('Escriba el número:')
    number = scan(n=1)
    
    url = paste0(u, Second$href[number])  
  }
  # sdata = read_html(iconv(url,to = "UTF-8"), encoding = "latin1")
  sdata = read_html(url)
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'/jaxi/Tabla')]"
                      , function(x){
                        data.frame(name = xmlValue(x, ignoreComments=T), href = xmlGetAttr(x, "href"))
                      })
  # links
  x = do.call(rbind.data.frame, links)
  Third = data.table(id = 1:nrow(x),x)
  
  
  
  # Fourth ------------------------------------------------------------------
  
  u <- 'http://www.ine.es'
  msg = as.character(paste(Third[[1]],':', Third[[2]]))
  print(msg)
  cat('Escriba el número')
  number = scan(n=1)
  
  url = paste0(u, Third$href[number])
  sdata = read_html(url) 
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'dlgExport.htm')]"
                      , function(x){
                        data.frame(name = xmlValue(x, ignoreComments=T), href = xmlGetAttr(x, "href"))
                      })
  # links
  x = do.call(rbind.data.frame, links)
  LINK = x$href
  
  
  # Se abre para descargar --------------------------------------------------
  
  u <- 'http://www.ine.es/jaxi/'
  url = paste0(u, LINK)
  sdata = read_html(url) 
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'xlsx')]"
                      , function(x){
                        data.frame(name = xmlValue(x, ignoreComments=T), href = xmlGetAttr(x, "href"))
                      })
  links
  x = do.call(rbind.data.frame, links)
  LINK2 = x$href
  
  url = paste0(u, LINK2)
  
  temp <- paste0(getwd(),'/f1.xlsx')
  download.file(url, temp, mode = 'wb')
  data = data.table(readxl::read_excel(temp)) 
  unlink(temp)
  
  #eliminaremos las primeras observaciones
  elim = 1:(min(which(is.na(data[[2]])==F))-1)
  data1 = data[!elim]
  header = as.character(unlist(data1[1,]))
  data1 = data1[-1]
  colnames(data1) = header
  #Lo mismo pero des de abajo
  for(i in length(data1[[2]]):1){
    if(is.na(data1[[2]])[i-1]==F){
      elim = i
      break
    }
  }
  data2 = data1[1:(elim-1)] 
  
  nms = c()
  v = c()
  for(i in 1:nrow(data2)){
    if(is.na(data2[[2]][i])){
      v = data2[[1]][i]
    }else{
      nm <- paste0(v, ifelse(is.null(v), '', '_'), data2[[1]][i])
      nms = c(nms, nm)
    }
  }
  
  
  data2 = data2[!is.na(data2[[2]])]
  
  dt <- data.table(header[-1], transpose(data2[,-1]))
  names(dt) <- c('Date', as.character(nms))
  dt = dt %>% arrange(Date) %>% as.data.table()
  dt[, names(dt)[-1] := lapply(.SD, as.numeric), .SD = names(dt)[-1]]
  dt$Date <- as.yearmon(dym(dt$Date))
  return(dt)
}
