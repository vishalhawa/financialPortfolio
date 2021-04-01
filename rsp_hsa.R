# Performance of RSP / TDF / HSA - VG funds 

library(tidyverse)
library(tidyquant)
library(data.table)
library(timetk)

rsp_hsa_adv <- readr::read_csv("rsp_hsa_adv.csv")


Ra <-rsp_hsa_adv %>% 
  select(ticker) %>% 
  tq_get( from = Sys.Date()-365*1, to   = Sys.Date()) %>%
  group_by(ticker) %>%
  tq_transmute(select  = adjusted,  mutate_fun = periodReturn,  period = "daily",  col_rename = "Ra")
Ra


Ra = BatchGetSymbols(rsp_hsa_adv$ticker,first.date = Sys.Date()-365*1,last.date = Sys.Date())$df.tickers
Ra = Ra[,c('ref.date', 'ticker', 'ret.adjusted.prices')] %>% as_tibble()

Ra %>%   group_by(ticker) %>% summarise(n = n()) %>% arrange(n)
Ra %>%   select(ticker) %>% uniqueN()
Ra = rename(Ra, returns = ret.adjusted.prices, date = ref.date)
## Charts

Ra %>%
  ggplot(aes(x = date, y = Ra)) +
  geom_bar(aes(color=ticker),stat = "identity", fill = palette_light()[[1]]) +
  # labs(title = " Returns", subtitle = "AAPL,  GOOG, and  NFLX",  caption = "Shows an above-zero trend meaning positive returns",  x = "", y = "Monthly Returns") +
  geom_smooth(method = "lm") +
  theme_tq() +
  scale_color_tq() +
  scale_y_continuous(labels = scales::percent)

Ra %>%  group_by(ticker) %>%
  tq_performance(Ra = returns,  Rb = NULL, performance_fun = SharpeRatio,p=.99,Rf= getQuote('^TNX')$Last/100)

Ra %>%  group_by(ticker) %>%
  tq_performance(Ra = returns, Rb = NULL, performance_fun = table.AnnualizedReturns) %>% 
  arrange(-`AnnualizedSharpe(Rf=0%)`)

Ra %>%group_by(ticker) %>%
  tq_performance(Ra = returns, Rb = NULL, performance_fun = VaR) %>% 
  arrange(-VaR)

## Max Drawn down
Ra %>%group_by(ticker) %>%
  tq_performance(Ra = returns, Rb = NULL, performance_fun = maxDrawdown) %>% 
  left_join(y=rsp_hsa_adv) %>% 
  arrange(-.[[2]]) %>% 
  head(10) %>% 
  ggplot(aes( y=.[[2]], x=reorder(ticker,-.[[2]]))) + 
  geom_bar(aes(fill=account), stat="identity", alpha=.6, width=.4) +
  theme_tq() + ylab('Max Drawdown %')



## Portfolio Analytics ---
ef = Ra %>% pivot_wider(date,names_from=ticker,values_from=returns) %>% tk_xts() %>% na.omit() %>% 
  generate_EF(npf = 400)
ef_line= summary(ef)[2][[1]]

## Ret-Risk _ EF
Ra %>%  group_by(ticker) %>%
  tq_performance(Ra = returns, Rb = NULL, performance_fun = table.AnnualizedReturns) %>%
  left_join(y=rsp_hsa_adv) %>% 
  ggplot(aes(x = AnnualizedStdDev, y = AnnualizedReturn)) +
  geom_point(aes(color = account ),size=4) +
  geom_text(aes(label=ticker),size=3, hjust = "inward") +
  geom_smooth(method = "lm") +
  geom_line( data= as.data.table(ef_line )[,.(AnnualizedReturn=250*mean,AnnualizedStdDev=sqrt(250)*StdDev)],
             aes(x = AnnualizedStdDev, y = AnnualizedReturn),linetype=2,color='blue') +
  labs(title = "Visualizing Returns Relationship of VG-Funds") +
  theme_tq()


ef_wts =as.data.table(summary(ef, digits=2)$weights)
         
# Stacked + percent Bar     
ef_wts[,colMeans(ef_wts )> 0, with=F] %>% 
  mutate(risklevel = 1:n()) %>% 
  pivot_longer(-risklevel) %>% 
ggplot(aes(fill=name, y=value, x=risklevel)) + 
  geom_bar(position="fill", stat="identity")

# chart.EfficientFrontier
ef_wts[,colMeans(ef_wts )!=0, with=F]
chart.EfficientFrontier(ef,rf=0,  match.col="StdDev", # which column to use for risk
                        type="l",   RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE)

## opt_maxret_roi
Ra %>% pivot_wider(date,names_from=ticker,values_from=returns) %>% tk_xts() %>% na.omit() %>% 
opt_maxret_roi()

Ra %>% pivot_wider(date,names_from=ticker,values_from=returns) %>% tk_xts() %>% na.omit() %>% 
  opt_maxret_roi() %>% 
chart.RiskReward(return.col="mean", risk.col="sd", chart.assets=TRUE,  main="ETF ALL: ROI Optimized")

