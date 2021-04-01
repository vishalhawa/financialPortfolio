 
################# Finance :  Portfolio Updates  ##################################

##### INput: Takes stock symbols from the database #
##### Process: Gets Key Stats & Options data from Yahoo URL / Query  ###
#### Process: Filter Options, Calculates Greeks and store into database/Portfolio #
##   *** NOTE: Works with only 32-bit R because of 32 bit Access ***


library(quantmod)
library(RODBC)
library(fOptions)
library(data.table)
library(stringr)
 
setwd(paste0("C:/Users/",Sys.getenv("username"),"/OneDrive/Projects/ML/finance-portfolio"))

utilpath = paste0('C:/Users/',Sys.getenv('username'),'/OneDrive/Projects//ML/utilities/util-finance.R')
dbpath =  paste0('C:/Users/',Sys.getenv('username'),'/OneDrive/Projects/FirmFinancials/USStocks.accdb')
# dbpath =  paste0('C:/Users/',Sys.getenv('username'),'/OneDrive/Projects/finance-portfolio/USStocks.accdb')

source(utilpath)

fetchOptions<-function(stock,exp=NULL){
  #  NOT IN USE CURRENTLY
  options=tryCatch(getOptionChain(Symbols=stock,Exp=exp), error=function(x){list(symbol=list(sym=data.frame( Strike=NA,  Last=NA,   Chg=NA,   Bid=NA,   Ask=NA, Vol=NA,   OI=NA))); })
  options<-do.call(rbind,lapply(options, function(x) do.call(rbind, x))) 
  options$OS<-gsub("^.*\\.","",rownames(options)) # option symbols
  options$stock <- gsub(pattern="[[:digit:]]*\\D[[:digit:]]*$","",options$OS)
  
  return (options)
}
# fetchOptions('BA')

applyOptionsFilter<-function(opList){
  rows.keep = which(options$OI>0)
    opList=opList[rows.keep,]
  return(data.table(opList))
}



########################## Driver ###############################################
# cat("Updating Share Stats in DB...")
# source('keyStats.R') # This is for Share Stats

print(getwd())

rf = as.numeric(getQuote('^TYX')[,c('Last','Volume')][,'Last'])/100
ch<-odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",dbpath))

# odbcConnect('USStocks32')


#### For India Portfolio  ##### ------------------------------------------------------------
cat('Getting  Ind-Stock and Update table ...','\n')
Inpf.tab <- data.table(sqlFetch(ch, "INPortfolioTable")) #Fetch Query results 
InTicker =  grep('(.BO$)|(.NS$)',unique(Inpf.tab$symbol) ,value = T)   
quotes = rbindlist(lapply(InTicker,function(x) list(symbol=x,ClosePrice=tryCatch(getSymbols(x,from=Sys.Date()-1,env = NULL)[1,6] ,error= function(e)  xts(NA,order.by = Sys.Date())) ) ))
setkey(quotes,'symbol')

quotes = Inpf.tab[quotes,on='symbol'][,.(symbol,ClosePrice,Remarks)]  # get all rows from Inpf.tab that match with quotes (even duplicates)
quotes$LastUpdated= Sys.Date()
# quotes=na.omit(quotes)
sqlUpdate(ch,dat=na.omit(quotes[Remarks=='S',!(Remarks:Remarks),with=FALSE]), tablename = 'INStocks',index = c('symbol'))
sqlUpdate(ch,dat=na.omit(quotes[Remarks=='MF',.(symbol,NAV=ClosePrice)]), tablename = 'INMutualFund',index = c('symbol'))


print("Updating IND-Portfolio.. DB...")
#### Update IND Portfolio -   ####
portfoliodf =  data.table(sqlQuery(channel = ch, "Select ID,p.symbol, p.Number, p.BuyPrice,BuyDate,s.ClosePrice ,f.NAV from ( INPortfolioTable p LEFT JOIN INStocks s ON s.symbol=p.symbol ) LEFT JOIN INMutualFund f ON f.symbol=p.symbol ORDER BY p.symbol",stringsAsFactors=FALSE,nullstring=0))
portfoliodf$Price = portfoliodf[,.(NAV+ClosePrice)] 

portfoliodf$Profit          = (portfoliodf$Price-portfoliodf$BuyPrice)*portfoliodf$Number
portfoliodf$AbsoluteGrowth  = (portfoliodf$Price/portfoliodf$BuyPrice -1)*(portfoliodf$Number/abs(portfoliodf$Number)) # Just to get the correct sign for shorts 
portfoliodf$AnnualizedGrowth = (portfoliodf$AbsoluteGrowth/(as.numeric(Sys.Date()-as.Date(portfoliodf$BuyDate))/365))
portfoliodf$CostBasis       = (portfoliodf$BuyPrice)*portfoliodf$Number*(portfoliodf$Number/abs(portfoliodf$Number))
portfoliodf$LastUpdated     = Sys.Date()

sqlUpdate(ch,dat=portfoliodf[,-c('ClosePrice','Price','NAV')],tablename="INPortfolioTable",index=c("ID"))  


###### For US Portfolio ####
pf.tab <- data.table(sqlFetch(ch, "USPortfolio")) #Fetch Query results 
setkey(pf.tab,symbol)

ticker = unique(gsub(pattern="[[:digit:]]*\\D[[:digit:]]{8}$","", pf.tab$symbol))
quotes = data.table(getQuote(ticker))[,.(symbol=ticker,ClosePrice=as.numeric(Last),Volume=as.numeric(Volume),LastUpdated=Sys.Date())] # Can be buggy if quote for ticker not get 
setkey(quotes,'symbol')

cat('updating US-Stock, USETF and Funds table ...','\n')
### Update USETF table #######   
sqlUpdate(ch,dat= quotes[symbol %in% pf.tab[Remarks == 'ETF']$symbol], tablename = 'USETF',index = c('symbol'))

### Update USStocks table #######   
sqlUpdate(ch,dat= quotes[symbol %in% pf.tab[Remarks == 'S']$symbol], tablename = 'USStocks',index = c('symbol'))

### Update USFunds table : 5 letter symbols ending with X #######
sqlUpdate(ch,dat= quotes[str_detect(quotes$symbol,pattern = '^.{4}X$'),.(symbol,NAV=ClosePrice,LastUpdated)], tablename = 'USFund',index = c('symbol'))

### Options Block #######
print("Fetching Options...")
op.symbols = unique(gsub(pattern="[[:digit:]]*\\D[[:digit:]]*$","", pf.tab[Remarks %in% c("BC","BP","SC","SP"),symbol]))
if(length(op.symbols)>0){
stkdata<-(t(sapply(op.symbols,function(x)sqlQuery(ch,paste0("select ClosePrice,Volatility20,Volatility,DividendYield from USStocks where symbol='",x,"'")))))
stkdata<-data.table(apply(stkdata,c(1,2),as.numeric))
stkdata$symbol<-op.symbols

options = rbindlist(lapply(op.symbols, getOptions),fill = T)[,1:13]
options <- merge(options,stkdata,by="symbol")
options <- cbind(options,apply(t(mapply(options$symbol ,FUN = calculateGreeks,stockPrice=options$ClosePrice,dailyVolatility=options$Volatility,dy=options$DividendYield/100,rf=rf)),c(1,2),as.numeric))
options$LastUpdated = Sys.Date()

# options=cbind(options,(t(mapply(options$OS,FUN = getOptionPrice,stkPrice=options$ClosePrice,yearlyVol=options$Volatility20))))
# apply(t(mapply(options$OS,FUN = getOptionPrice,stkPrice=options$ClosePrice,yearlyVol=options$Volatility20)),c(1,2),as.numeric)

print("Inserting Options in DB...")
sqlDrop(ch, "USOptionsTable") ; # sqlClear(ch, "USOptionsTable") NOT WORK #sqlQuery(channel, 'Delete * from USOptionsTable')
sqlSave(ch, options, tablename="USOptionsTable",rownames = FALSE, varTypes = c("LastUpdated"="date","expiry"="date","lastTradeDate"="date"),verbose = F)
}

print("Updating US-Portfolio.. DB...")

#### Update US Portfolio ####
portfoliodf =  data.table(sqlQuery(channel = ch, "Select ID, p.symbol, p.Number, p.BuyPrice,BuyDate,s.ClosePrice ,f.NAV , ot.price  from
                                            (( USPortfolio p LEFT JOIN USStocks s ON s.symbol=p.symbol ) LEFT JOIN USFund f ON f.symbol=p.symbol)
                                             LEFT JOIN USOptionsTable ot ON ot.OS=p.symbol ORDER BY p.symbol",stringsAsFactors=FALSE,nullstring=0))
etf_tbl <- data.table(sqlFetch(ch, "USETF"))[pf.tab[Remarks=='ETF'],on='symbol'][,.(ID,symbol,Number ,BuyPrice,  BuyDate,Price=ClosePrice)]

set(portfoliodf,i=portfoliodf[is.na(price),which=T],j='price',value = 0)  # # Make NAs to 0 #
set(portfoliodf,i=portfoliodf[is.na(ClosePrice),which=T],j='ClosePrice',value = 0)  # # Make NAs to 0 #
portfoliodf = portfoliodf[,':='(Price=(NAV+ClosePrice+price))][Price!=0,.(ID, symbol, Number, BuyPrice , BuyDate ,  Price)]

portfoliodf = rbind(etf_tbl,portfoliodf) 

portfoliodf$Profit = (portfoliodf$Price-portfoliodf$BuyPrice)*portfoliodf$Number
portfoliodf$AbsoluteGrowth = (portfoliodf$Price/portfoliodf$BuyPrice -1)*(portfoliodf$Number/abs(portfoliodf$Number)) # Just to get the correct sign for shorts 
portfoliodf$AnnualizedGrowth = portfoliodf$AbsoluteGrowth/(as.numeric(Sys.Date()-as.Date(portfoliodf$BuyDate))/365)
portfoliodf$CostBasis = (portfoliodf$BuyPrice)*portfoliodf$Number*(portfoliodf$Number/abs(portfoliodf$Number)) # Just to get the correct sign for shorts 
portfoliodf$LastUpdated = Sys.Date()

sqlUpdate(ch,dat=portfoliodf[,-c('ClosePrice','Price','NAV','price')],tablename="USPortfolio",index=c("ID"))

#### Update US Executed ##############################################################################################
print("Updating US-Executed.. DB...")
### Fetch/Insert/ Sell Orders to USExecuted table###

sell_tx = pf.tab[!is.na(SellPrice),.(TxID=seq(from=as.numeric(Sys.time()),length.out=.N),ID,Symbol=as.character(symbol),Options=symbol,Number,BuyPrice,BuyDate,SellPrice,SellDate,AnnualizedGrowth=0,AbsoluteGrowth=SellPrice/BuyPrice-1,Profit=Number*(SellPrice-BuyPrice),Commission,Type=Remarks,CostBasis,LastUpdated=Sys.Date(),Account)] 
if(nrow(sell_tx)>0){
  sqlSave(ch,sell_tx[,-('ID'),with=F],'USExecuted',append = T,rownames = F,fast = F) #varTypes = c("Type"="char",'SellPrice'='double','BuyDate'='date','SellDate'='date'))
  sapply(sell_tx$ID,function(x)sqlQuery(ch,paste0("delete Symbol,Options,Shares  from USPortfolio where ID=",x)))
}

close(ch)

# Commission not accounted ..
con = DBI::dbConnect(odbc::odbc(), driver = "{Microsoft Access Driver (*.mdb, *.accdb)}",  Dbq   = "../../FirmFinancials/USStocks.accdb") ## OK 
executed<- data.table(DBI::dbReadTable(con,'USExecuted'))

executed$Profit = (executed$SellPrice-executed$BuyPrice)*executed$Shares
executed$AbsoluteGrowth = (executed$SellPrice/executed$BuyPrice -1)*(executed$Shares/abs(executed$Shares)) # Just to get the correct sign for shorts 
executed$AnnualizedGrowth = executed$AbsoluteGrowth/(as.numeric(Sys.Date()-as.Date(executed$BuyDate))/365)
executed$CostBasis = (executed$BuyPrice)*executed$Shares*(executed$Shares/abs(executed$Shares)) # Just to get the correct sign for shorts 

DBI::dbWriteTable(con, "USExecuted",executed, batch_rows = 1,overwrite  = TRUE) ## this is inflating file size ..


## Update WORSKHEET -----------------------------------------------------------
print("Updating Worksheet ..")
wb <- openxlsx::loadWorkbook("Finance.xlsx")  
names(wb)
openxlsx::removeWorksheet(wb, 'pf_raw')
openxlsx::removeWorksheet(wb, 'executed_raw')
openxlsx::removeWorksheet(wb, 'pf_ind_raw')
openxlsx::removeWorksheet(wb, 'executed_ind_raw')
openxlsx::addWorksheet(wb, "pf_raw")
openxlsx::addWorksheet(wb, "executed_raw")
openxlsx::addWorksheet(wb, "pf_ind_raw")
openxlsx::addWorksheet(wb, "executed_ind_raw")
openxlsx::writeData(wb,sheet='pf_raw',data.table(DBI::dbReadTable(con, "USPortfolio")))
openxlsx::writeData(wb,sheet='executed_raw',data.table(DBI::dbReadTable(con, "USExecuted")))
openxlsx::writeData(wb,sheet='pf_ind_raw',data.table(DBI::dbReadTable(con, "INPortfolioTable")))
openxlsx::writeData(wb,sheet='executed_ind_raw',data.table(DBI::dbReadTable(con, "INExecuted")))

openxlsx::saveWorkbook(wb, "Finance.xlsx",overwrite = T) 

DBI::dbDisconnect(con) # executed is processed..

print("Closing.. DB...End : Update Portfolio")




## EOF ---- OPTION Processing --
# # extract Dates
# as.Date(gsub("*\\D[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)","",c("CVX160311C00077000","V160422C00045000"))),format = "%y%m%d")
# # Strikes
# as.numeric(gsub(pattern="(^[[:alpha:]]+)\\d{6}[[:alpha:]]","",c("CVX160311C00074500","V160422C00045000")))/1000
# # Type
# (gsub("[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)\\d{6}","",c("CVX160311C00077000","V160422P00045000"))))

  # lapply("USO", fetchOptions)
# getOptionPrice("V160617P00070000",71.63,0.015,dy=0.0079,rf=0.0267)
# Symbol
# gsub(pattern="([[:alnum:]]){15}$","",c("CVX160311C00077000","V160422C00045000"))
# Treasury Yield 30 Years (^TYX)