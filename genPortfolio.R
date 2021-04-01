 
############## Finance :  Portfolio Optimization: HSA 401K  RSP ###################
# ref: https://rpubs.com/mxiu47/pf_opt_txg
# https://www.rpubs.com/hgjerning/517730
# https://github.com/R-Finance/PortfolioAnalytics/blob/master/demo/demo_efficient_frontier.R
# Factor Model: https://lf0.com/post/factor-modeling-for-portfolio-optimisation/factor-modeling-portfolio-optimisation/
######## IN: Takes stock symbols from the database ##
######## Process: Calculates Asset Returns  ###
######## Process: Calulates Sotino Ratio and optimal weights maxRet/minVar ##
######### ML Algo: - None ##
######### OUT: Various Sortinio Ratio and Plots ###
# library(DBI) ; con <- dbConnect(odbc::odbc(), "USStocks32")

library(data.table)
library(magrittr)
library(BatchGetSymbols)
library(tidyquant)
source('utils.R')

# Run optimization - method = random -- eg ----
pf = {
  p = portfolio.spec(assets = colnames(hsaRet)) 
  p = add.objective(portfolio = p, type = "risk", name = "StdDev",target=0.002)
  p = add.objective(portfolio = p, type = "return", name = "mean",target=0.005)
  p = add.constraint(portfolio = p, type = "weight_sum",min_sum=0.99 , max_sum=1.01)
  p =  add.constraint(portfolio=p, type="long_only")
  p
}

hsa_opt_rn <- optimize.portfolio(R = hsaRet, portfolio = pf, optimize_method = 'random',rp = random_portfolios(pf, 5000, 'sample'), trace = TRUE)
plot(hsa_opt_rn, main = 'Random Optimized Base Portfolio',    risk.col = 'StdDev', neighbors = 10)
chart.RiskReward(hsa_opt_rn, chart.assets = TRUE,return.col = "mean", risk.col = "StdDev")


hsa_ef = create.EfficientFrontier( R = hsaRet,  portfolio = {
                          p = portfolio.spec(assets = colnames(hsaRet)) 
                          p = add.objective(portfolio = p, type = "return", name = "mean")
                          p = add.objective(portfolio = p, type = "risk", name = "StdDev")
                          p = add.constraint(portfolio = p, type = "weight_sum",min_sum=0.99 , max_sum=1.01)
                          p =  add.constraint(portfolio=p, type="long_only")
                          p
                          }
                          ,  type = "mean-sd", n.portfolios = 25, )

summary(hsa_ef, digits=2)
chart.EfficientFrontier(hsa_ef,
                        match.col="StdDev", # which column to use for risk
                        type="l", 
                        RAR.text="Sharpe Ratio",
                        tangent.line = FALSE,
                        chart.assets=TRUE,
                        labels.assets=TRUE)

### Visual Analyze --------------------------------------------------------------------

# plot() ; chart.Concentration() ;  chart.EfficientFrontier() ; chart.RiskReward() ; chart.RiskBudget() ; chart.Weights() - for single period or rebal
# 
# extractObjectiveMeasures(); extractWeights()  ; extractStats() ; 
## Return.portfolio for compute rebalancing returns
# charts.PerformanceSummary()

con <- dbConnect(odbc::odbc(), "USStocks32")
days = 365*2 # 2 yrs worth of data
rf =  getQuote('^TNX')$Last/100

## **NEW *** Driver: Analyze  Funds by Accts ---------------------------------------------------------------------------------

nyrs= 3 ; rf =  getQuote('^TNX')$Last/100  ## get returns for 1,3,5
## Global returns/ef
rsp_hsa_adv <- readxl::read_excel("data/rsp_hsa_adv.xlsx")
ef_all = rsp_hsa_adv %>% 
          nest_by(account) %>% 
          mutate(returns = list(getReturns(data$symbol,days = 365*nyrs)) ) %>% 
          mutate(ef =  list(generate_EF(returns,npf=100))  )

## only need if tidyquant.
  # select(symbol, date, close) %>% group_by(symbol)  %>% 
  # tq_transmute(select=close,  mutate_fun = periodReturn,  period = "daily",  col_rename = "returns") %>% 
  # as.data.table %>%  dcast( date ~ symbol) %>% .[-1] %>% as.xts 

## For single year ..

ef_all$ef[[1]] %>%  chart.EfficientFrontier(chart.assets=T,match.col='StdDev')  ## std plot 

stats = ef_all %>% 
            transmute( stats = list(rbind(sapply(returns, mean)*(365),sapply(returns, sd)*sqrt(365) ) %>%
                                t %>% as.data.table(keep.rownames = T)  %>% 
                                setnames(c('symbol','mean','sd'))
                          )) %>%  .$stats

## as expected TDFs align with EF of Advice 
p = ef_plot(ef_all$ef[[1]])
p = p +  geom_point(data=stats[[4]], aes(x=sd,y=mean),col='darkgreen')  + 
  geom_text(data=stats[[4]],  aes(x=sd,y=mean,label= symbol),vjust = "outward" ,col='darkgreen')
 p
 plotly::ggplotly( p  )


## For multiple years --TBD
advice_ef_nyr = map_dfr(list(t1=1,t3=3,t5=5), ## uses ret global ..
                        function(x) { front=  ret[index(ret)>Sys.Date()-x*365] %>% generate_EF(npf=100)  %$% frontier 
                        front = as.data.table(matrix(front,nrow = nrow(front)) ) 
                        names(front) <-  c('mean','StdDev','out',colnames(ret))
                        front$mean = front$mean*365  ; front$StdDev = front$StdDev*sqrt(365)
                        front$sr = front$mean/front$StdDev
                        front
                        },.id = 'nyrs')  

rets =    map_dfr(list(t1=1,t3=3,t5=5), ## uses ret global ..
                 function(x) { front=  ret[index(ret)>Sys.Date()-x*365] } ,.id = 'nyrs') %>% 
  group_by(nyrs) %>% 
  summarise(across(where(is.numeric),list(mean= ~mean(.x)*365,sd= ~sd(.x)*sqrt(365)),.names = "{.col}_{.fn}")) %>% 
  pivot_longer(-nyrs) %>% 
  separate(name,c('symbol','stat')) %>% 
  pivot_wider(names_from = stat,values_from = value)

# rets = t(rbind(sapply(ret,mean)*365 ,sapply(ret,sd)*sqrt(365)) ) %>% as.data.table(keep.rownames = T)
ggplot(advice_ef_nyr, aes(x= StdDev, y= mean, col=nyrs)) + geom_line() + labs(title = "EF Frontier") + theme_linedraw() +
  geom_point(data=rets, aes(x=sd,y=mean,col=nyrs))  +    geom_text(data=rets,  aes(x=sd,y=mean,label= symbol),vjust = "outward" ) 

## Max drawdown ------------
ef_all$returns[[1]] %>% as_tibble(rownames='date')

ef_all %>% 
  mutate(mxdd = list(as_tibble(returns,rownames='date') %>% 
                          mutate(date = as.Date(date))  %>%  
                          pivot_longer(2:last_col(),names_to = 'symbol',values_to='ret_long')%>% 
                          group_by(symbol) %>% 
                          tq_performance(Ra=ret_long,performance_fun=maxDrawdown) ##  
                   )
         ) 




## ** NEW ** Analyze VG ETF & VG FUNDS #### --------------------------------------

vgetf = data.table( dbReadTable(con,'USStocks ETF family'))
vgfunds = dbReadTable(con,'USFund by family')
vg = c(vgetf$symbol , vgfunds$symbol, 'SPY') #
## OR Get from file ..
vgetf <- read_excel("data/vgetf.xlsx", col_types = c("text", "text", "numeric", "numeric", "text",  "numeric", "text"))
vgfunds <- read_excel("data/vgetf.xlsx", sheet = "vgfund", col_types = c("numeric", "text", "text", "numeric", "numeric", "text", "text"))

vg_etf_funds =  bind_rows(vgetf %>% mutate(type = 'etf'),   vgfunds %>% mutate(type = 'funds')  ) %>%   mutate(mcap = parse_number(mcap) )  # str_remove(mcap,'\\$| billion(| )$')

nyrs = 2
ef_vg_etf_funds = 
  vg_etf_funds %>% 
  nest_by(type) %>% 
  mutate(  returns = list(getReturns(data$symbol,days = 365*nyrs))) %>% 
  mutate( ef_curve = list(generate_EF(returns))) %>% 
  mutate( plot = list(ef_plot(data,returns,ef_curve  ) ))

ef_vg_etf_funds[1,] %>%   mutate( plot = list(ef_plot(data,returns,ef_curve ,xvar = 'StdDev' , yvar = 'mean' ) ))

## Old ..
vgRet = getReturns(vg,days = 250) ## number of rows/days cannot be less than number of cols/sym
cat('dim: ',dim(vgRet))

## funds only 
vg_ef_funds = generate_EF(vgRet[j=vgfunds$symbol])
chart.EfficientFrontier(vg_ef_funds,rf=rf,  match.col="StdDev", # which column to use for risk
                        type="l",   RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE)

vg_ef = generate_EF(ret=vgRet)

opt_maxret_roi(vgRet)
chart.RiskReward(opt_maxret_roi(vgRet),return.col="mean", risk.col="sd", chart.assets=TRUE,  main="VG ALL: ROI Optimized")

## EF--
vg_ef_wts = as.data.table(summary(vg_ef, digits=2)$weights)
vg_ef_wts[,colMeans(vg_ef_wts>0)!=0, with=F]
chart.EfficientFrontier(vg_ef,rf=rf,  match.col="StdDev", # which column to use for risk
                        type="l",   RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE)


## Plot of weights by time : maxret  note 90 days 
vgWeights = do.call(rbind,lapply(tail(index(vgRet),90),function(idx) {wts = opt.Weights(vgRet[index(vgRet)<=(idx),],type = 'maxRet')$weights;data.table(symbol=names(wts),wts,Date=idx) } )) 
ggplot(vgWeights[wts>0.05],aes(x=Date))+geom_line(aes(y=wts,col=symbol))  + geom_text( data=vgWeights[, g:=.GRP,by=Date][g==max(g)&wts>0.05], aes(label = symbol, x = Date, y = wts), hjust = -.1) + scale_colour_discrete(guide = 'none')  
plot(opt.Weights(vgRet,type = 'maxRet'), risk.col="StdDev", return.col="mean", main="Max return  Optimization", chart.assets=TRUE)

## Min Var - Safe
vgWeights = do.call(rbind,lapply(tail(index(vgRet),90),function(idx) {wts = opt.Weights(vgRet[index(vgRet)<=(idx),],type = 'minVar')$weights;data.table(symbol=names(wts),wts,Date=idx) } )) 
ggplot(vgWeights[wts>0.05],aes(x=Date))+geom_line(aes(y=wts,col=symbol))  + geom_text( data=vgWeights[, g:=.GRP,by=Date][g==max(g)&wts>0.05], aes(label = symbol, x = Date, y = wts), hjust = -.1) + scale_colour_discrete(guide = 'none')  
plot(opt.Weights(vgRet,type = 'minVar'), risk.col="StdDev", return.col="mean", main="Min Var  Optimization", chart.assets=TRUE)

### ** NEW: ALL Sector Performance  ####------------------------------------------------------------------------
long_days = 365*3 ; short_days = 90
sector_ind <- readxl::read_excel("data/etf_sector_ind.xlsx", col_types = c("text", "text", "text",  "numeric", "skip", "numeric", "skip", "skip", "text"))
sector_ind %>% group_by(industry) %>% summarise(n(),sum(assets)) %>% arrange(`sum(assets)`)

sector_ef = sector_ind %>% 
          group_by(industry) %>% 
          dplyr::filter(assets == max(assets)) %>%  ungroup() %>%  ## only max of group
          select(symbol) %>% 
          tq_get(from = Sys.Date()-short_days,to=Sys.Date())  %>%   # list(getReturns(symbol,days = short_days))
        select(symbol, date, close) %>% group_by(symbol)  %>% 
        tq_transmute(select=close,  mutate_fun = periodReturn,  period = "daily",  col_rename = "returns") %>% 
         as.data.table %>%  dcast( date ~ symbol) %>% .[-1] %>% 
        generate_EF()  

  chart.EfficientFrontier(  sector_ef, match.col="StdDev",  type="l", RAR.text="Sharpe Ratio",  tangent.line = T,  chart.assets=TRUE, labels.assets=TRUE)
 
  short_days = 60
  rbind(sapply(  sector_ef$R %>% tail(short_days), mean), sapply(  sector_ef$R %>% tail(short_days), sd)) %>%
    t %>% as.data.table(keep.rownames = T) %>% 
    merge(sector_ind,all.x=T,by.x = 'rn',by.y = 'symbol') %>% 
    ggplot(aes(x=V2,y=V1,color=sector))+geom_jitter(alpha=0.5)+theme_tq() +
    geom_text(aes(label=rn),vjust = "outward")

# macro_sectors = c('spy','xle','xlk','xlf','xly','xlp','xlv','xli','xlb','xme','xhb','vnq','xlu')
# energy = c('xle','amlp','vde','amj','emlp','xop','oih','spy')
# consumer_disc = c('xly','vcr','iyc','fdis','xrt')
# sectors = c(energy,consumer_disc)
# sectors = macro_sectors

ret_long = na.omit(ROC(do.call(merge,lapply(sectors,function(x) Ad(getSymbols(x,from = Sys.Date()-long_days,env=NULL))))))
colnames(ret_long) = gsub('.Adjusted','',colnames(ret_long))
ret_short = na.omit(ROC(do.call(merge,lapply(sectors,function(x) Ad(getSymbols(x,from = Sys.Date()-short_days,env=NULL))))))
colnames(ret_short) = gsub('.Adjusted','',colnames(ret_short))


plot(opt_weights(ret_long,type = 'maxRet'), risk.col="StdDev", return.col="mean", main="Maximum Return Optimization-Long", chart.assets=TRUE)
plot(opt_weights(ret_short,type = 'maxRet'), risk.col="StdDev", return.col="mean", main="Maximum Return Optimization-Short", chart.assets=TRUE)


## Driver: All Large ETFs #### ---------------------------------------------------------------------------------------

stkdata = fread('https://onedrive.live.com/download?cid=13C9A0CA3A18CA60&resid=13C9A0CA3A18CA60%2129848&authkey=AJjLop0fuLHPwDU') # USStocks.csv
all_etfs = union(stkdata[Sector=='ETF' & MCap >1000]$symbol, c(stkdata[Sector=='ETF' & Volume >1000]$symbol,'UUP','TVIX') )

## dont needt leveraged etf.. + above will not work ksince etf is not there anymore..

etf_ret = getReturns(all_etfs ,days = 250) ## number of rows/days cannot be less than number of cols/sym
cat('dim: ',dim(etf_ret))

ef_etfs = generate_EF(etf_ret) ; ef_etf_wt = as.data.table(summary(ef_etfs, digits=2)$weights)
ef_etf_wt[,colMeans(ef_etf_wt > 0)!=0, with=F]
ef_etf_wt[,colMeans(ef_etf_wt )!=0, with=F]
chart.EfficientFrontier(ef_etfs,rf=rf,  match.col="StdDev", # which column to use for risk
                        type="l",   RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE)

opt_maxret_roi(etf_ret)
chart.RiskReward(opt_maxret_roi(etf_ret),return.col="mean", risk.col="sd", chart.assets=TRUE,  main="ETF ALL: ROI Optimized")

## Driver: Current PF Allocations #### --------------------------------------------------------------------

pf = data.table( dbReadTable(con,'USPortfolio'))
# ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../FirmFinancials/USStocks.accdb")
# pf <- data.table(sqlFetch(ch, "USPortfolio"))  
ticker = unique(as.character(pf[Remarks%in%c('S','MF')]$symbol))
 units = pf[,.(units=sum(Number)),by=symbol]$units

quotes =  getReturns(ticker)
 
tik_ret = as.xts(na.omit(dcast(as.data.table(quotes$df.tickers),  ref.date ~ ticker, value.var  = 'ret.closing.prices')))

# quotes = lapply(ticker,function(x) Ad(getSymbols(x,from = Sys.Date()-days,env=NULL)))
# Adjusted_price <-do.call(merge,quotes)
# 
units.m = matrix(data=rep(units,nrow(Adjusted_price)),nrow=nrow(Adjusted_price),ncol=ncol(Adjusted_price),byrow = T)
weights=Adjusted_price*units.m/rowSums(Adjusted_price*units.m) # Stock units is assumed to have remained constant - NOT TRUE
plot(weights)
#Get Returns of portfolio
# returns.portfolio <- na.omit(ROC(Adjusted_price))
# colnames(returns.portfolio) = gsub('.Adjusted','',colnames(returns.portfolio))

#### Calculate SortinoR with changing portfolio weights
sr = do.call(rbind,lapply(tail(index(weights),30),function(idx) xts(order.by = idx,SortinoRatio(returns.portfolio[index(returns.portfolio)<=(idx),],weights = as.numeric(weights[idx]))) ))

returns.SPY = ROC(Ad(getSymbols('SPY',from = Sys.Date()-days,env=NULL)))
sr.SPY = do.call(rbind,lapply(tail(index(returns.SPY),30),function(idx) xts(order.by = idx,SortinoRatio(returns.SPY[index(returns.SPY)<=(idx),])) )) # weights NULL

# Do Portfolio OPTM - MAX RET
opt_maxret = opt.Weights(tik_ret,type = 'maxRet' )
# opt_maxret=opt.Weights(returns.portfolio,type = 'maxRet')
# opt_maxret
plot(opt_maxret, risk.col="StdDev", return.col="mean", main="Maximum Return Optimization", chart.assets=TRUE)#, xlim=c(0, 0.02), ylim=c(0,0.003))
  
 optWts = do.call(rbind,lapply(tail(index(returns.portfolio),30),function(idx) opt.Weights(returns.portfolio[index(returns.portfolio)<=(idx),])$weights))
 optWts = xts(optWts,order.by = tail(index(returns.portfolio),30))  

## Optimal SR based on optimal Weights - maxRet   
opt.sr.maxRet = do.call(rbind,lapply(index(optWts),function(idx) xts(order.by = idx,SortinoRatio(returns.portfolio[index(returns.portfolio)<=(idx),],weights = as.numeric(optWts[idx]))) ))

## minVar
SortinoRatio(returns.portfolio,weights = opt.Weights(returns.portfolio,type = 'minVar')$weights)
optWts.minVar = do.call(rbind,lapply(tail(index(returns.portfolio),30),function(idx) opt.Weights(returns.portfolio[index(returns.portfolio)<=(idx),],type = 'minVar')$weights))
optWts.minVar = xts(optWts.minVar,order.by = tail(index(returns.portfolio),30))  

opt.sr.minVar = do.call(rbind,lapply(index(optWts.minVar),function(idx) xts(order.by = idx,SortinoRatio(returns.portfolio[index(returns.portfolio)<=(idx),],weights = as.numeric(optWts.minVar[idx]))) ))


# Plot OPtimal SR and Actual SR ####
autoplot(sr)+geom_line(aes(y=opt.sr.maxRet,color='maxRet'),linetype=2)+geom_line(aes(y=opt.sr.minVar,color='minVar'),linetype=2)+geom_line(aes(y=sr.SPY,color='SPY'))+
  ylab('SR')+ labs(caption = "(based on data from ..Y!)")+theme_light()+theme(legend.position='bottom') + scale_colour_manual('PF Type',breaks = c("maxRet", "minVar", "SPY"),values = c("red", "green3", "blue")) 

close(ch)

## Analyse VG ETF ONLY ####--------------------------------------------------------------------
 library(DBI) ; con <- dbConnect(odbc::odbc(), "USStocks32")
# sql = "SELECT USStocks.symbol, USStocks.sector, Round(USStocks.volatility,2) AS Vty, Round(dividendYield,2) AS Yield, Round(dividendGrowth,2) AS dG, USStocks.revenue, USStocks.ClosePrice, USStocks.PS, Round(gain*dG,1) AS signal, Round(USStocks.gain-1,2) AS gain, name, USStocks.Beta, USStocks.Industry FROM USStocks WHERE USStocks.Name like 'Vanguard*'"
vgetf = data.table( dbReadTable(con,'USStocks ETF family'))
# etfRet = getReturns(vgetf$symbol[1:69]) ## removing last 7 factor funds - not enough history

# OR
symbols = fread('https://onedrive.live.com/download?cid=13C9A0CA3A18CA60&resid=13C9A0CA3A18CA60%2126289&authkey=AMeP07QRGNHyV4o')
setkey(symbols,symbol)
vgetf = symbols[Name %like% 'Vanguard',.(symbol)]

etf_ret = getReturns(c(vgetf$symbol,'SPY'),days = 150)

cat('\n','Number of Rows: ',dim(etf_ret),'\n')

ef_etf = generate_EF(etf_ret)
colMeans(etf_ret)[colMeans(etf_ret)> colMeans(etf_ret)['SPY']] ## beating SP

## EF--
ef_etf_wts = as.data.table(summary(ef_etf, digits=2)$weights)
ef_etf_wts[,colMeans(ef_etf_wts>0)!=0, with=F]  ## prints only non-zero columns
chart.EfficientFrontier(ef_etf,rf=rf,  match.col="StdDev", # which column to use for risk
                        type="l",   RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE,main = 'EF: VG-ETF 4 months')



etfWeights = do.call(rbind,lapply(tail(index(etfRet),90),function(idx) {wts = opt.Weights(etfRet[index(etfRet)<=(idx),],type = 'maxRet')$weights;data.table(symbol=names(wts),wts,Date=idx) } )) 

## Plot of weights by time : maxret  note 90 days 
ggplot(etfWeights[wts>0.05],aes(x=Date))+geom_line(aes(y=wts,col=symbol))  + geom_text( data=etfWeights[, g:=.GRP,by=Date][g==max(g)&wts>0.05], aes(label = symbol, x = Date, y = wts), hjust = -.1) + scale_colour_discrete(guide = 'none')  

plot(opt.Weights(etfRet,type = 'maxRet'), risk.col="StdDev", return.col="mean", main="Max return  Optimization", chart.assets=TRUE)

# Also need to calculate SR ratio for both return types 
plot(opt.Weights(etfRet,type = 'minVar'), risk.col="StdDev", return.col="mean", main="Min Var  Optimization", chart.assets=TRUE)



## Analyse VG FACTOR FUNDS #### -------------------------------------------------------------------------------------------------------------
library(DBI)

vgff = c('VGT','SPY','VFMV','VFVA','VFMO','VFLQ','VFQY','VFMF','VFMFX')
# vg = c(vgetf$symbol[30:69] ,'SPY')
vgff = getReturns(vgff,th=0)
vgRet = vgff 
paste('# of Rows : ',nrow(vgRet),' :', max(index(vgRet)))


plot(opt_weights(vgRet,type = 'maxRet',method = 'random'),
     risk.col="StdDev", return.col="mean", main="Maximum Return Optimization-VG FF", chart.assets=TRUE)

## Plot of weights by time : maxret  note 250 days 
vgWeights = do.call(rbind,lapply(tail(index(vgRet),250),function(idx) {wts = opt.Weights(vgRet[index(vgRet)<=(idx),],type = 'maxRet')$weights;data.table(symbol=names(wts),wts,Date=idx) } )) 
ggplot(vgWeights[wts>0.05],aes(x=Date))+geom_line(aes(y=wts,col=symbol))  + geom_text( data=vgWeights[, g:=.GRP,by=Date][g==max(g)&wts>0.05], aes(label = symbol, x = Date, y = wts), hjust = -.1) + scale_colour_discrete(guide = 'none')  
plot(opt.Weights(vgRet,type = 'maxRet'), risk.col="StdDev", return.col="mean", main="Max return: VG Factor Funds", chart.assets=TRUE)

## Analyse VG IRA  FUNDS #### --------------------------------------------------------------------------------------------------
library(DBI); con <- dbConnect(odbc::odbc(), "USStocks")
vgira = data.table( dbReadTable(con,'USFund'))[FundFamily%like%'Vanguard'& Rem=='IRA']$symbol
vgira = unique(c(vgira,'VGT','VPMAX','VIGIX'))
vgRet = getReturns(vgira)
cat('Dim: ',dim(vgRet))  ##  

## Plot of weights by time : maxret  note 250 days 
vgWeights = do.call(rbind,lapply(tail(index(vgRet),250),function(idx) {wts = opt.Weights(vgRet[index(vgRet)<=(idx),],type = 'maxRet')$weights;data.table(symbol=names(wts),wts,Date=idx) } )) 
ggplot(vgWeights[wts>0.05],aes(x=Date))+geom_line(aes(y=wts,col=symbol))  + geom_text( data=vgWeights[, g:=.GRP,by=Date][g==max(g)&wts>0.05], aes(label = symbol, x = Date, y = wts), hjust = -.1) + scale_colour_discrete(guide = 'none')  
plot(opt.Weights(vgRet,type = 'maxRet'), risk.col="StdDev", return.col="mean", main="Max return: IRA", chart.assets=TRUE)

opt_weights(vgRet,type = 'maxRet',method = 'random') %>% chart.EfficientFrontier(match.col = 'StdDev') ## method ROI not working 

## Analyse  VG FUNDS  ONLY #### --------------------------------------
vgfunds2 = dbReadTable(con,'USFund by family')
## OR
vgfunds2 = fread('vgfunds.csv')
vgRet = getReturns(c(vgfunds2$symbol,'SPY'),days = 250) ## number of rows/days cannot be less than number of cols/sym
cat('dim: ',dim(vgRet))

colMeans(vgRet)[colMeans(vgRet)> colMeans(vgRet)['SPY']] ## beating SP
## funds only 
vg_ef_funds = generate_EF(vgRet)
chart.EfficientFrontier(vg_ef_funds,rf=rf,  match.col="StdDev", # which column to use for risk
                        type="l",   RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE)

vg_ef_wts = as.data.table(summary(vg_ef_funds, digits=2)$weights)
vg_ef_wts[,colMeans(vg_ef_wts>0)!=0, with=F]


##### EOF ####
