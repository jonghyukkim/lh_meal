
# load packages

suppressMessages({
  library(glue)
  library(dplyr)
  library(httr)
  library(rjson)
  library(stringr)
})



api.key <- 'l1GeQXeoXWkn94FuokbdC6%2FfMjTgZRdhKIZViFX%2FYS%2BGTHmScCar4BC%2BxD%2BK5mRXaWbWj1Iz5Z%2Fa5WEenEQSuw%3D%3D'

url.format <- 'http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?serviceKey={key}&solYear={year}&solMonth={month}'



holiday.request <- function(key, year, month) glue(url.format)
holidays <- data.frame(name=character(), day=integer(), stringsAsFactors=FALSE)

# request and read data

for(y in 2016:2021){
  for(m in 1:12){
    
    items <- 
      holiday.request(api.key, y, str_pad(m, 2, pad=0)) %>% 
      GET %>%
      content(as='text', encoding='UTF-8') %>%
      fromJSON %>%
      .$response %>%
      .$body %>%
      .$items
    
    if(items != ''){ # empty items check
      if(is.null(names(items$item))){ # many item case
        for(i in 1:length(items$item)){
          item <- items$item[[i]]
          if(item$isHoliday == 'Y'){
            
            holidays <- 
              holidays %>% 
              rbind(data.frame(name=item$dateName, day=item$locdate, stringsAsFactors=FALSE))
          }
        }
      }else{ # just 1 item case
        
        item <- items$item
        
        if(item$isHoliday == 'Y'){
          holidays <- 
            holidays %>% 
            rbind(data.frame(name=item$dateName, day=item$locdate, stringsAsFactors=FALSE))
        }
      }
    }
  }
  
  holidays <- holidays %>%
    rbind(data.frame(name='노동절', day=paste(y, '0501', sep='')))
}



holidays <- holidays %>% mutate(day = as.integer(as.character(day))) %>% arrange(day) 

holidays <- holidays %>% 
  mutate(Date = ymd(day)) %>% 
  select(Holiday_name = name, Date)
  
write.csv(holidays, file='holidays.csv', fileEncoding='UTF-8', row.names=FALSE)
