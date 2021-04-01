## utils

### Util Functions ----------------------------------------
library(PortfolioAnalytics)

getReturns <- function(tiks,period='daily',days = 365*3,th=0.75 ){
   future::plan(future::multisession, workers = floor(parallel::detectCores()/2))
  # future::plan(future::sequential())
  quotes =  BatchGetSymbols(tickers = tiks,  first.date =  Sys.Date()-days,last.date = Sys.Date(), bench.ticker='SPY' ,
                            do.cache=F,freq.data = period, do.parallel = T  , thresh.bad.data=th)
  quotes = as.xts(dcast(as.data.table(quotes$df.tickers),  ref.date ~ ticker, value.var  = 'ret.adjusted.prices'))
  # quotes = lapply(symbols,function(x) na.omit(Ad(getSymbols(x,from = Sys.Date()-days,env=NULL))) )
  # Adjusted_price <-do.call(merge,quotes)
  # if (period=='daily'){
  #   returns.portfolio <- na.omit(ROC(Adjusted_price))
  # }else{
  #     returns.portfolio <- do.call(merge,lapply(Adjusted_price, function(p) periodReturn(na.omit(p),period = period) ))
  # }
  # # colnames(returns.portfolio) = gsub('.Adjusted','',colnames(returns.portfolio))
  # colnames(returns.portfolio) = symbols
  # return(returns.portfolio)
  na.omit(quotes)
}
# getReturns(all_etfs[1:10])

opt_weights<-function(pf.ret ,method='ROI',target=NULL){
  p <- portfolio.spec(assets = colnames(pf.ret))   
  p = add.objective(portfolio = p, type = "return", name = "mean",target=target)
  p = add.objective(portfolio = p, type = "risk", name = "StdDev")
  p <- add.constraint(portfolio = p, type = "weight_sum",min_sum=0.99 , max_sum=1.01)
  # global minimum variance portfolio=optimization therefore returns the portfolio weights which attain the lowest possible portfolio variance.  
  # If you would like to get the efficient portfolio then you need to add an additional constraint, namely the minimal expected return. 
  p<- add.constraint(portfolio=p, type="long_only")
  # p <- add.constraint(portfolio=p, type = "return", return_target=20/250) # 20 per cent per year in expectation.
  opt_pf = optimize.portfolio(R=pf.ret, portfolio = p, optimize_method = method, trace = T)
  opt_pf
}
# opt_pf = opt_weights(ef_all$returns[[1]],method = 'random',target = 0.01) 
## (default)ROI seems to max returns while random->risk!!
# extractWeights(opt_pf); extractStats(opt_pf)
# chart.EfficientFrontier(opt_pf,match.col = "StdDev")
# chart.RiskReward(opt_pf,chart.assets=TRUE)

opt_rebalancer<-function(pf_ret ,method='ROI',rebalance_on = 'quarters'){
  opt_rebal <- optimize.portfolio.rebalancing(R = pf_ret, portfolio = {
    p = portfolio.spec(assets = colnames(pf_ret) )
    p = add.objective(portfolio = p, type = "return", name = "mean")
    p = add.objective(portfolio = p, type = "risk", name = "StdDev")
    p = add.constraint(portfolio = p, type = "weight_sum",min_sum=0.99 , max_sum=1.01)
    p =  add.constraint(portfolio=p, type="long_only")
    p
  }, optimize_method = method,trace = TRUE, search_size = 1000 ,rebalance_on = rebalance_on, training_period = 60, rolling_window = 60)
  extractWeights(opt_rebal)
}
# opt_rebalancer(ef_all$returns[[1]],method='random')

opt.Weights <- function(pf.ret,type='maxRet' ,method='ROI'){  ## ROI *** Not in use ***
  p <- portfolio.spec(assets = colnames(pf.ret))   # Now, whenever you refer to the variable p you can change your portfolio specifications. 
  p=  switch(type,
             maxRet=add.objective(portfolio = p, type = "return", name = "mean"),
             minVar=add.objective(portfolio = p, type = "risk", name = "var"),
             stop('Objective ill-defined, should be maxRet or minVar'))
  # # The full_investment part is equivalent to setting a constraint on the sum of the portfolio weights such that they always sum-up to 1. 
  p <- add.constraint(portfolio = p, type = "full_investment")
  # global minimum variance portfolio=optimization therefore returns the portfolio weights which attain the lowest possible portfolio variance.  
  # If you would like to get the efficient portfolio then you need to add an additional constraint, namely the minimal expected return. 
  p<- add.constraint(portfolio=p, type="long_only")
  # p <- add.constraint(portfolio=p, type = "return", return_target=20/250) # 20 per cent per year in expectation.
  #Optimize
  opt_pf = optimize.portfolio(R=pf.ret, portfolio = p, optimize_method = method, trace = TRUE)
  opt_pf
  return(opt_pf)
}
# opt.Weights(ret_long,type = 'maxRet') ## this is method ROI
 
generate_EF <- function(ret,method = "random",npf=100){
pf =   opt_weights(ret,method = method) %>% 
        extractEfficientFrontier( match.col='StdDev',n.portfolios = npf)
pf
}
# generate_EF(ef_all$returns[[1]],npf=100,method = "ROI") 

ef_plot <- function(ef,df=NULL) { ### Need to extract EF first ##
  rets = ef$R
  front = ef$frontier; 
  front = as.data.table(matrix(front,nrow = nrow(front)) ) 
  names(front) <-  c('mean','StdDev','out',colnames(rets))
  front$mean = front$mean*365  ; front$StdDev = front$StdDev*sqrt(365)
  front$sr = front$mean/front$StdDev
  rets = t(rbind(sapply(rets,mean)*365 ,sapply(rets,sd)*sqrt(365)) ) %>% as.data.table(keep.rownames = T)
  p =  ggplot(front, aes(x= StdDev, y= mean)) + geom_line(aes(color= sr )) + labs(title = "EF Frontier") + theme_linedraw()+
    geom_point(data=rets, aes(x=V2,y=V1),col='blue') + geom_text(data= rets,  aes(x=V2,y=V1,label=rn),vjust = "outward" , col='blue') +
    xlab('Risk->')+ylab('Returns')
  # print(p)
  p
}

###  