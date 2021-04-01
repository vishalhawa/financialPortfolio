## performance of TDF  & Retirement income funds over $1B 
# 
# tdfpage <- read_html("https://mutualfunds.com/categories/all-funds/strategy-funds/target-date-funds/#tm=1-fund-category&r=Channel%23412&only=meta%2Cdata&selected_symbols=&page=2") %>%
#   html_nodes("tr") %>%
#   html_text()
# 
# parse_number(tdfpage)

library(data.table)
library(rvest)
library(readr)
library(stringr)

tdf <- readxl::read_excel("tdf.xlsx") %>% as.data.table()
setnames(tdf,c('fund_name','symbol','company','category','Billions','1year','rank','expense','tenure'))
tdf = tdf[,1:5]
tdf$Billions = round(tdf$Billions/1000,2)

tdf$tdf_year = parse_number(str_remove( tdf$fund_name,'\\.|-'))
tdf[,tdf_year := ifelse((tdf_year-2019)>0,tdf_year,NA)]
tdf$share_class = str_remove( tdf$symbol ,str_extract(tdf$symbol,'\\(.*\\)') ) %>% str_remove_all('\\(|\\)')
tdf$symbol = str_extract(tdf$symbol,'\\(.*\\)') %>% str_remove_all('\\(|\\)')

glimpse(tdf)
tdf[,.N,.(company,category)][order(company)]
tdf[,.N,tdf_year]


tdf_ret = getReturns(tdf$symbol,days = 400)
keep_cols = names(colSums(is.na(tdf_ret))[colSums(is.na(tdf_ret)) <2]) ## filter bad funds
tdf_ret = na.omit(tdf_ret[, keep_cols])

tdf = tdf[symbol %in% keep_cols]
 
## Eff Frontier ...
plot_ef <- function(ret){
   ef = generate_EF(ret)
   dat = data.table(y=colMeans(ret),x= apply(ret, 2, sd),label=names(colMeans(ret)))
   dat = merge(dat,tdf[,.(symbol,company,Billions)],by.x='label',by.y='symbol',all.x=T)
   p = ggplot(as.data.frame(summary(ef)$metrics),aes(y=mean,x=StdDev))+geom_line(linetype=2) + 
      geom_point(data=dat,aes(x=x,y=y,size=Billions,color=company),alpha=0.3) +
      geom_text(data = dat, aes(x=x,y=y,label=label),size=3,color='blue',nudge_y = 0.000001) +
      theme_bw()  + scale_fill_viridis(discrete = TRUE) 
   p 
}


## Maxdrawdown --
plot_maxdrawdown <- function(ret){
   round(maxDrawdown(ret,invert = F),4) %>% t %>%
      as.data.table(keep.rownames = T) %>%
      merge(tdf[,.(symbol,company,Billions)],by.x = 'rn',by.y='symbol',all.x=T) %>% 
      .[order(`Worst Drawdown`)] %>% 
      .[,.SD[.N],company] %>%
      ggplot(aes(x=reorder(company, `Worst Drawdown`, FUN = max),y=`Worst Drawdown`)) + 
      geom_bar(color = "black", aes(fill = company), stat = "identity",alpha=0.3,width = 0.5) + theme_bw() +
       xlab('funds') + theme(axis.text.x=element_text(angle=45,hjust=1)) +  scale_fill_brewer(palette = "Dark2")
}

pdf('Max_draw_downs.pdf')
lapply(list(
   plot_maxdrawdown(tdf_ret[,tdf[is.na(tdf_year)]$symbol]) + labs(title = 'Worst Drawdown in Retirement'),
   plot_maxdrawdown(tdf_ret[,tdf[tdf_year %in% c('2020','2025') & Billions>2]$symbol])+labs(title = 'TDF: 2020/2025')), 
   print )
dev.off()


pdf('in_retirement.pdf')
lapply(list(
   plot_ef(na.omit(tdf_ret[,tdf[is.na(tdf_year)]$symbol])) + labs(title = 'Funds In Retirement') ,
   plot_maxdrawdown(tdf_ret[,tdf[is.na(tdf_year)]$symbol]) + labs(title = 'Worst Drawdown in Retirement')), 
   print )
dev.off()


pdf('tdf_2020_2025.pdf')
lapply(list(
   plot_ef(tdf_ret[,tdf[tdf_year %in% c('2020','2025') & Billions>2]$symbol])+labs(title = 'TDF: 2020/2025'),
   plot_maxdrawdown(tdf_ret[,tdf[tdf_year %in% c('2020','2025') & Billions>2]$symbol])+labs(title = 'TDF: 2020/2025')), 
   print )
dev.off()






ggplot(round(maxDrawdown(tdf_ret[,tdf[is.na(tdf_year)]$symbol],invert = F),4), aes(x=symbol, y=len, color=dose)) +   geom_bar(stat="identity", fill="white")
table.DownsideRisk(tdf_ret[,tdf[is.na(tdf_year)]$symbol])
ES(tdf_ret[,tdf[is.na(tdf_year)]$symbol], p=.95, method="historical")
ES(tdf_ret[,tdf[is.na(tdf_year)]$symbol], p=.95, method="modified")
chart.StackedBar(tdf_ret[,tdf[is.na(tdf_year)]$symbol])
## --

 #tdf_ef_wt = as.data.table(summary(tdf_ef, digits=2)$weights)
 chart.EfficientFrontier(generate_EF(tdf_ret_na),rf=NULL,  match.col="StdDev", type="l",  RAR.text="Sharpe Ratio",  tangent.line = F,  chart.assets=TRUE, labels.assets=TRUE)

 opt_maxret_roi(tdf_ret) %>% class
 chart.RiskReward(opt_maxret_roi(tdf_ret),return.col="mean", risk.col="sd", chart.assets=TRUE,  main="TDF ALL: ROI Optimized")
 
 colSums(!is.finite(tdf_ret))
 colSums(is.na(tdf_ret))
 temp = extractEfficientFrontier( opt_maxret_roi(tdf_ret[,1:10]), match.col = "StdDev", n.portfolios = 100)
 



 
 