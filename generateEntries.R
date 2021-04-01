### Prepare Order Entries from Broker #####

library(quantmod)
# library(RODBC)
library(fOptions)
library(data.table)
library(stringr)
library(BatchGetSymbols)
library(tidyverse)

# setwd(paste0("C:/Users/",Sys.getenv("USER"),"/OneDrive/Projects/ML/finance-portfolio"))

today_trades <- readxl::read_excel("today_trades.xlsx", 
                           col_types = c("text", "skip", "text", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric"))
today_trades = janitor::clean_names(today_trades)


pf <- readxl::read_excel("Finance.xlsx", sheet = "pf_raw",  
                      col_types = c("text", "numeric", "numeric", 
                                    "date", "numeric", "date", "skip", 
                                    "skip", "numeric", "skip", "skip", 
                                    "skip", "text", "date", "numeric", 
                                    "numeric", "text", "numeric"))


## entries to Open
trades<- today_trades %>% 
          mutate(symbol = ifelse(commission==0,symbol,  str_c(word(symbol,1),word(symbol,2),word(symbol,3),str_pad(1000*as.numeric(word(symbol,4)),8,pad = '0') )) , 
                 # quantity = if_else(commission==0,quantity,100*quantity),
                 action =  if_else( symbol %in% pf$symbol,'close','open'),
                 )  %>% 
          mutate( Remarks = if_else( action=='open', 
                                   str_c(str_extract(transaction_type,'^[[:alpha:]]{1}'), ## B|S
                                                  gsub("[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)\\d{6}","",symbol))),
                                   NA_character_),
                  quantity = if_else(commission!=0 & Remarks %in% c('SP','SC'),-100*quantity,100*quantity)
                  ) %>% 
          dplyr::filter(action=='open') %>% 
          rename(Number=quantity,BuyPrice=price) %>% 
          mutate(BuyDate=Sys.Date(), Commission = securities_transaction_fee+commission) %>% 
          select(symbol,Number, BuyPrice,BuyDate, Remarks,Commission)
        ## TBD adjust commission to cost basis in upateportfolio.R
print(trades)

## Dump to excel
print("Updating Worksheet ..")

openxlsx::write.xlsx(trades,file = "today_trades.xlsx",asTable = T,sheetName='today-trades')
openxlsx::openXL(file = "today_trades.xlsx")


# wb <- openxlsx::loadWorkbook("today_trades.xlsx")  
# names(wb)
# openxlsx::removeWorksheet(wb, 'trades')
# openxlsx::addWorksheet(wb, "trades")
# openxlsx::writeData(wb,sheet='trades',trades)
# openxlsx::saveWorkbook(wb, "today_trades.xlsx",overwrite = T)

