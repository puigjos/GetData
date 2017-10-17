get_BDE <- function(){
  require(dplyr)
  require(data.table)
  require(rvest)
  require(curl)
  require(xml2)
  require(stringr)
  require(XML)
  
  url <- 'https://www.bde.es/webbde/es/estadis/infoest/bolest.html'
  
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }
  
  
  sdata = read_html(url)
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'bolest')]"
                      , function(x){
                        data.frame(name = xmlValue(x, ignoreComments=T), href = xmlGetAttr(x, "href"))
                      })
  
  x = do.call(rbind.data.frame, links)
  First = data.table(id = 1:nrow(x),x)
  First = First[!grepl('^A', name)]
  First$name <- trim(gsub('[\n\r\t]', '', First$name))
  First$name <- sub('.*\\-', '', First$name)
  
  
  # Second part -------------------------------------------------------------
  
  u <- 'https://www.bde.es/webbde/es/estadis/infoest/bolest' 
  
  msg = as.character(paste(First[[1]],':', First[[2]]))
  print(msg)
  cat('Escriba el número')
  number = scan(n=1)
  
  url <- paste0(u, number, '.html')
  
  sdata = read_html(url) %>% html_node('#ctr') %>% html_text()
  
  x <- stringr::str_split(sdata, pattern = '\n')[[1]]
  x <- gsub('[\n\r\t]', '', x)
  x1 = x[grepl('^[0-9]', x)][-1]
  dts <- data.table(name = trim(sub('.*\\-', '', x1)), number = trim(gsub( "-.*$", "", x1)))
  
  sdata = read_html(url)
  sdata = htmlParse(sdata,encoding="UTF-8")
  links <- xpathApply(sdata, "//a[contains(@href,'.csv')]"
                      , function(x){
                        data.frame(name = xmlValue(x, ignoreComments=T), href = xmlGetAttr(x, "href"))
                      })
  
  x = do.call(rbind.data.frame, links)
  Second = data.table(id = 1:nrow(x),x)
  Second$name <- trim(gsub('[\r\n]','', Second$name))
  Second$number <- word(Second$name, -1)
  Second$name <- NULL
  Second = dts %>% inner_join(Second, by='number')
  
  
  msg = as.character(paste(Second[[3]],':', Second[[1]]))
  print(msg)
  cat('Escriba el número')
  number = scan(n=1)
  
  
  u <- 'https://www.bde.es'
  url <- paste0(u, Second$href[number])
  
  temp <- tempfile()
  download.file(url, temp)
  data = data.table(read.csv(temp))
  unlink(temp)
  
  header = data[2,]
  data = data[!1:3,]
  colnames(data) = c('Date', as.character(unlist(header))[-1])
  data$Date = zoo::as.yearmon(data$Date)
  data[, names(data)[-1] := lapply(.SD, function(x){as.numeric(as.character(x))}),
       .SD = names(data)[-1]]
  return(data)
}
dt <- get_BDE()
