#######  Template - PortfolioAnalytics ######
library(PerformanceAnalytics)
library(PortfolioAnalytics)

data(edhec)
stocks <- edhec[, 1:6]
init.portfolio <- portfolio.spec(assets = colnames(stocks))
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "full_investment") ## means weights sum =1
init.portfolio = add.constraint(portfolio=init.portfolio, type="long_only")
init.portfolio = add.objective(portfolio = init.portfolio, type = "return", name = "mean")
eq_pf = equal.weight(R=stocks,portfolio = init.portfolio,optimize_method = 'random')
plot(eq_pf) ## error 
opt_pf = optimize.portfolio(R= na.omit(stocks), portfolio = init.portfolio, optimize_method = 'DEoptim', trace = TRUE)
# The following constraint types are supported:
#   leverage
# box
# group
# position_limit1
# turnover2
# diversification
# return
# factor_exposure
# transaction_cost2

# Note that the name argument in add.objective can be any valid R function. Several functions are provided in the PerformanceAnalytics package that can be specified as the name argument such as ES/ETL/CVaR, StdDev, etc.
# The following objective types are supported:
#   return
# risk
# risk_budget
# weight_concentration
## More Objectives: Portfolio Optimization Objectives
# Minimize Risk: Volatility, Tail Loss (VaR, ES), Max Drawdown, etc.
# Maximize Risk Adjusted Return: Sharpe Ratio, Information Ratio, etc.
# Risk Budgets: Equal Component Contribution to Risk (i.e. Risk Parity), Limits on Component Contribution
# Maximize a Utility Function: Quadratic, Constant Relative Risk Aversion (CRRA), etc. can be any R fun
# Minimize Tracking Error; Replicating portfolio to track an index or basket of assets

## Generation - random portfolios
# sample
# simplex (FEV hyperparameter - fan out)
# grid

### optimize.portfolio to run the optimizations. The examples below use optimize_method="ROI", but several other solvers are supported including the following:
#   DEoptim (differential evolution)
# GenSA (generalized simulated annealing)
# pso (particle swarm optimization)
## ROI (R Optimization Infrastructure)
# Rglpk
# quadprog
# Symphony (Rsymphony)
# can be parellelized 

## Visual
plot(minSD.opt, risk.col="StdDev", chart.assets=TRUE,    main="Min SD Optimization",   ylim=c(0, 0.0083), xlim=c(0, 0.06))
# plot   ##	extractObjectiveMeasures
# chart.Concentration  ##	extractStats
# chart.EfficientFrontier	##extractWeights
# chart.RiskReward	##print
# chart.RiskBudget	##summary
# chart.Weights	

## Simple Example ####

portf.dn <- portfolio.spec(stocks)
# Add constraint such that the portfolio weights sum to 0*
portf.dn <- add.constraint(portf.dn, type="weight_sum",    min_sum=-0.01, max_sum=0.01)
# Add box constraint such that no asset can have a weight of greater than # 20% or less than -20%
portf.dn <- add.constraint(portf.dn, type="box", min=-0.2, max=0.2)
# Add constraint such that we have at most 20 positions
portf.dn <- add.constraint(portf.dn, type="position_limit", max_pos=20)
# Add constraint such that the portfolio beta is between -0.25 and 0.25
betas <- t(CAPM.beta(equity.data, market, Rf))
portf.dn <- add.constraint(portf.dn, type="factor_exposure", B=betas,   lower=-0.25, upper=0.25)

# Add objective to maximize portfolio return with a target of 0.0015
portf.dn.StdDev <- add.objective(portf.dn, type="return", name="mean",   target=0.0015)
# Add objective to minimize portfolio StdDev with a target of 0.02
portf.dn.StdDev <- add.objective(portf.dn.StdDev, type="risk", name="StdDev",   target=0.02)

# Generate random portfolios
rp <- random_portfolios(portf.dn, 10000, "sample") ## can be sample/simplex/grid
# Run the optimization
opt.dn <- optimize.portfolio(equity.data, portf.dn.StdDev,   optimize_method="random", rp=rp,   trace=TRUE)
## OR
opt <- optimize.portfolio(R, portfolio=p, optimize_method='random', search_size=2000)
opt.rebal <- optimize.portfolio.rebalancing(R, portfolio=p,
                                            optimize_method='random',
                                            search_size=2000,
                                            rebalance_on='quarters',
                                            training_period=60,
                                            rolling_window=60)

plot(opt, main="Dollar Neutral Portfolio", risk.col="StdDev", neighbors=10)


### Mininum Expected Shortfall #####
# Consider an allocation to hedge funds using the EDHEC-Risk Alternative Index as a proxy. This
# will be an extended example starting with an objective to minimize modified expected shortfall,
# then add risk budget percent contribution limit, and finally add equal risk contribution limit.
# Minimize Modified Expected Shortfall
# Minimize Modified Expected Shortfall with Risk Budget Limit
# Minimize Modified Expected Shortfall with Equal Risk Contribution

# Specify an initial portfolio
funds <- colnames(R)
portf.init <- portfolio.spec(funds)
# Add constraint such that the weights sum to 1* 
portf.init <- add.constraint(portf.init, type="weight_sum",  min_sum=0.99, max_sum=1.01)
# Add box constraint such that no asset can have a weight of greater than # 40% or less than 5%
portf.init <- add.constraint(portf.init, type="box",   min=0.05, max=0.4)
# Add return objective with multiplier=0 such that the portfolio mean # return is calculated, but does not impact optimization
portf.init <- add.objective(portf.init, type="return",    name="mean", multiplier=0)

# Add objective to minimize expected shortfall
portf.minES <- add.objective(portf.init, type="risk", name="ES")
# Add objective to set upper bound on percentage component contribution
portf.minES.RB <- add.objective(portf.minES, type="risk_budget",  name="ES", max_prisk=0.3)
# Relax box constraints
portf.minES.RB$constraints[[2]]$max <- rep(1,ncol(R))
# Add objective to minimize concentration of modified ES component contribution
portf.minES.EqRB <- add.objective(portf.minES, type="risk_budget",  name="ES", min_concentration=TRUE)
# Relax box constraints
portf.minES.EqRB <- add.constraint(portf.minES.EqRB, type="box", min=0.05, max=1, indexnum=2)

# Combine the 3 portfolios
portf <- combine.portfolios(list(minES=portf.minES,
                                 minES.RB=portf.minES.RB,
                                 minES.EqRB=portf.minES.EqRB))
# Run the optimization
opt.minES <- optimize.portfolio(R, portf, optimize_method="DEoptim",  search_size=5000, trace=TRUE, traceDE=0)

# Set Rebalancing Parameters and Run Backtest
# Set rebalancing frequency
rebal.freq <- "quarters"
# Training Period
training <- 120
# Trailing Period
trailing <- 72
bt.opt.minES <- optimize.portfolio.rebalancing(R, portf, optimize_method="DEoptim",  rebalance_on=rebal.freq,  training_period=training, trailing_periods=trailing,
                                               search_size=5000,
                                               traceDE=0)

ret.bt.opt <- do.call(cbind, lapply(bt.opt.minES, function(x) summary(x)$portfolio_returns))
colnames(ret.bt.opt) <- c("min ES", "min ES RB", "min ES Eq RB")
charts.PerformanceSummary(ret.bt.opt)
# Chart Weights Through Time
chart.Weights(bt.opt.minES, main=" Weights", col=bluemono)

# Example 3: Maximize CRRA
# Consider an allocation to hedge funds using the EDHEC-Risk Alternative Index as a proxy. Our
# objective to maximize the fourth order expansion of the Constant Relative Risk Aversion (CRRA)
# expected utility function as in the Boudt paper and Martellini paper. We use the same data as

# http://past.rinfinance.com/agenda/2014/workshop/RossBennett.pdf

### EXAMPLE #####
# Baseline portfolio specification to minimize portfolio standard deviation, subject to full investment and long only constraints.
rp.seq <- generatesequence(min = 0, max = 1, by = 0.002) # sequence of possible weights
p <- portfolio.spec(assets = colnames(R),   weight_seq = rp.seq)
p <- add.constraint(portfolio = p, type = 'weight_sum',     min_sum = 0.99, max_sum = 1.01)
p <- add.constraint(portfolio = p, type = 'box', min = 0, max = 1)
p <- add.objective(portfolio = p, type = 'return', name = 'mean',    multiplier = 0)
p <- add.objective(portfolio = p, type = 'risk', name = 'StdDev')

# portfolio specification with box constraints
p.box <- p
p.box <- add.constraint(portfolio = p.box, type = 'box', min = 0.05, max = 0.20, indexnum = 2)

# generate the random portfolios
rp <- random_portfolios(portfolio = p, permutations = rp.n, method = 'sample')
rp <- normalize.weights(rp)
rp.box <- random_portfolios(portfolio = p.box, permutations = rp.n, method = 'sample')
rp.box <- normalize.weights(rp.box)

# run the optimizations - Single period
opt.base <- optimize.portfolio(R = tail(R, 36), portfolio = p, rp = rp, optimize_method = 'random', trace = TRUE)
opt.box <- optimize.portfolio(R = tail(R, 36), portfolio = p.box, rp = rp.box, optimize_method = 'random',  trace = TRUE)

## Rebalance
opt.base.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p, optimize_method = 'random', rp = rp, trace = TRUE,  rebalance_on = 'quarters',
                                                 training_period = 36, rolling_window = 36)
opt.base.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.base.rebal))
colnames(opt.base.rebal.r) <- 'base'


opt.box.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p.box,  optimize_method = 'random',rp = rp.box, trace = TRUE,  rebalance_on = 'quarters',
                                                training_period = 36,  rolling_window = 36)

opt.box.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.box.rebal))  ## evauluate returns 
colnames(opt.box.rebal.r) <- 'box'
  chart.Weights(opt.base.rebal, main = 'Baseline Portfolio Optimal Weights')
  
  ### Perfromance ####
  opt.r <- cbind(opt.base.rebal.r, opt.box.rebal.r)
  charts.PerformanceSummary(opt.r, main = "Performance Summary")
  
  ##### Custom Objective Function ####
  ## Tracking error
  # A few guidelines should be followed for defining a custom objective function.
  #  The objective function must return a single value for the optimizer to minimize.
  #  It is strongly encouraged to use the following argument names in the objective function:  R for the asset returns  weights for the portfolio weights
  
  te.target <- function(R, weights, Rb, min.te = 0.02, max.te = 0.05, scale = 12){
    r <- Return.portfolio(R = R, weights = weights)
    Rb <- Rb[index(r)] ## baseline returns 
    te <- sd(r - Rb) * sqrt(scale)  ## looks like volatility for rolling time 
    # penalize tracking error outside of [min.te, max.te] range
    out <- 0
    if(te > max.te) out <- (te - max.te) * 10000
    if(te < min.te) out <- (min.te - te) * 10000
    out  ## to be minimized 
  }
  # Add tracking error as an objective to the baseline portfolio specification
  # Specify the arguments to te.target as a named list
  # 
  
  p.te <- p  ## copy base portfolio ## type Risk to minimize ??
  p.te <- add.objective(portfolio = p, type = 'risk', name = 'te.target', arguments = list(Rb = R.mkt, scale = 12, min.te = 0.03, max.te = 0.05))
  
  opt.te.rebal <- optimize.portfolio.rebalancing(R = R, portfolio = p.te, optimize_method = 'random',   rp = rp, trace = TRUE,
                                                 rebalance_on = rebal.period,   training_period = n.train,   rolling_window = n.roll) 
  opt.te.rebal.r <- Return.portfolio(R, weights = extractWeights(opt.te.rebal))  ## calculated weighted returns 
  colnames(opt.te.rebal.r) <- 'te.target'
  
  opt.r <- na.omit(cbind(opt.base.rebal.r, opt.box.rebal.r, opt.te.rebal.r, R.mkt))
  charts.PerformanceSummary(opt.r, main = "Performance Summary")
  
