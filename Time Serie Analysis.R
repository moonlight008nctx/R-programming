# Time Serie Analysis

## start scripts: 1) load packages needed; 2) set the work directory; 3) connect to database 
pkgs <- c("forecast", "sweep", "timekit", "tidyquant", "geofacet")
install.packages(pkgs)
library(DBI)
library(data.table)
library(broom)
library(ggplot2)
library(sandwich)
library(regclass)
library(forecast)
library(tseries)
library(sweep)
library(timekit)
library(tidyquant)
library(geofacet)
setwd("C:/Users/xdli/Dropbox/UTD/2021/Fall/BA with R/R - Fall 2021-wb")
db_name <- 'wooldridge2.db'
con <- dbConnect(RSQLite::SQLite(),db_name)
dbListTables(con) # shows all the table in dababase "wooldridge2"


## User-defined functions
  # Function 1 --> This function pulls target table from a specific db. Note that the database connection was established before this function. 
  # This function takes in argument of table name as string 'table_name'.
tb_pull <- function(tablename){
  dt <- dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(dbReadTable(con, paste0(tablename,'_labels')))
  return(dt)
}

  # Function 2 -->  the following model does the same thing as regular tidy() for a linear regression model. But this is needed to build tidy.w and tidy.nw
tidy.g <- function(model,vc=vcov(model),conf.int=FALSE,conf.level=0.95){
  dt <- tidy(model,conf.int=conf.int,conf.level=conf.level)
  dt$std.error <- sqrt(diag(vc))
  dt$statistic <- dt$estimate/dt$std.error
  dt$p.value <- 2*pnorm(-abs(dt$statistic))
  if(conf.int){
    dt$conf.low <- dt$estimate+qnorm((1-conf.level)/2)*dt$std.error
    dt$conf.high <- dt$estimate-qnorm((1-conf.level)/2)*dt$std.error
  }
  return(dt)
}
  # Function 3 --> the following model will do the Heteroscedasticity-Consistent covariance matrix. This is essentially the 
  # vcovHC() computes Heteroscedasticity-consistent estimation of the covariance matrix of the coefficient estimates
tidy.w <- function(model,...)tidy.g(model,vc=sandwich::vcovHC(model),...)

  # Function 4 --> the following model will do the Heteroscedasticity and autocorrelation consistent (HAC) covariance matrix estimation.
tidy.nw <- function(model,...)tidy.g(model,sandwich::NeweyWest(model),...)


## pull data from database using the tb_pull function. Plot data.
minwage <- tb_pull('minwage')
  # subset of data from table minwage.Note that this table does not contain the time column
  # time column will be added 
minwage2 <- minwage[,.(emp232,wage232,unem,cpi,minwage)]

  # plot time series in r.When you plot it, the x axis says 'time', which is actually the row number of data. 
ts.plot(minwage2$emp232)
ts.plot(minwage2$wage232)
ts.plot(minwage2$cpi)
  # let's add the time factor in the data table. In reality this data should always be clarified, but in this dataset, the time is added based on CPI historical value, and back calculate thet correponding time range, of course estimated.
  # 'time' is a column contains data that is float - 1950+1/12 refers to the 1st month of 1950. 
minwage2$time <- seq(1950,2001-1/12,1/12)
  # It appears that ts.plot(column) does not recognize the time column, but instead would use the row number as time point.  
  # so a better option would be ggplot() which specifies the columns of x and y.
  # First scatter plot of emp232 vs time
ggplot(minwage2,aes(x=time,y=emp232)) + geom_point() +
  scale_x_continuous('Year (Monthly)') +
  scale_y_continuous('Employment in Thousands')
  # we can also do line linking all the points. 
ggplot(minwage2,aes(x=time,y=emp232)) + geom_line() +
  scale_x_continuous('Year (Monthly)') +
  scale_y_continuous('Employment in Thousands')

##  ALERT: Spurious model! The model below may looks reasonable, but it is totally non-sense. All variables need to be confirmed as stationary first. 
model <- lm(emp232~wage232+time,data=minwage2)
summary(model)  
    # Coefficients:
    #   Estimate Std. Error t value Pr(>|t|)    
    # (Intercept) -1.809e+04  4.027e+02  -44.91   <2e-16 ***
    #   wage232     -6.838e+01  1.410e+00  -48.51   <2e-16 ***
    #   time         9.428e+00  2.061e-01   45.75   <2e-16 ***
    #   ---
    #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    # 
    # Residual standard error: 21.36 on 609 degrees of freedom
    # Multiple R-squared:  0.7949,	Adjusted R-squared:  0.7942 
    # F-statistic:  1180 on 2 and 609 DF,  p-value: < 2.2e-16

## Check stationary, and stationary transformation by differencing. 
  # we use kpss.test to detect whether a variable is level stationary or not. 
  # grammer of kpss.test --> kpss.test(x, null = c("Level", "Trend"), lshort = TRUE). Level is default. lshort = TRUE is default
  # null hypothesis of kpss test --> H_0: employment is level stationary (with no differencing)
  # If we fail to reject the hypothesis, we can proceed with ts modeling, because there is no indication that data is not stationary.  
kpss.test(minwage2$emp232)  
kpss.test(minwage2$emp232,null="Trend")
kpss.test(diff(minwage2$emp232))
kpss.test(diff(minwage2$emp232),null="Trend")


  # as you can tell this gets a bit trivia. To save effort, we can design a function to do kpss.test
  # in the following function, test is conducted with difference degree of 0 to 5 for both level and trend stationary feature.
  # we have to suppress the warning, or else we may get multple warinings for small p values. 
  # ultimately, this function output the degree of differencing is needed to reach a stationary stage, and whether it is trend stationary or level stationary. 
rep.kpss <- function(series){
  for(diffs in 0:5){
    suppressWarnings(
      pval <-kpss.test(series)$p.value
    )
    if(pval>0.05){return(c(diffs,"level",pval))}
    suppressWarnings(pval <-kpss.test(series,null="Trend")$p.value)
    if(pval>0.05) {return(c(diffs,"trend",pval))}
    series <- diff(series)
  }
  stop("Was never stationary")
}
  # when we run the rep.kpss function on both emp232 and wage232, we know that both need to be differenced by 1 to be trend stationary.
rep.kpss(minwage2$emp232)
rep.kpss(minwage2$wage232)
      # > rep.kpss(minwage2$emp232)
      # [1] "1"     "trend" "0.1"  
      # > rep.kpss(minwage2$wage232)
      # [1] "1"                 "trend"             "0.077915960964795"

  # the alternative way to define a kpss function. 
  # rep.kpss <- function(series,max.reps=5,level=0.95,trend=TRUE){
  #   # start with a list holding three 0 s. 
  #   obj <- list(diffs = 0, trend=0, p.value=0)
  #   # not sure why we need to define the class of obj
  #   class(obj) <- "repkpss"
  #   for(i in 0:max.reps){
  #     suppressWarnings(pval <- tseries::kpss.test(series)$p.value)
  #     if(pval >= 1-level){
  #       obj$diffs <- i
  #       obj$trend <- 0
  #       obj$p.value <- pval
  #       return(obj)
  #     }
  #     # trend=0
  #     if(trend){
  #       suppressWarnings(pval <- tseries::kpss.test(series,null="Trend")$p.value)
  #       if(pval >= 1-level){
  #         obj$diffs <- i
  #         obj$trend <- 1
  #         obj$p.value <- pval
  #         return(obj)
  #       }
  #     }
  #     series <- diff(series)
  #   }
  # }

## Time series modeling ARIMA
  # Based on the learnings from rep.kpss, we know that we have to differenced the two variable by 1
  # and since both variables are trend stationary, we need to include time as one of the explanatory variable. 
  # note when you difference variable, you will need to adjust the time column so it would remove the
  # top row(s) (top row does not have previous period data to be differenced with)
  # If degree of difference is d, then time range would be time[(d+1):nrow(data.table)]
modelb <- lm(diff(emp232)~diff(wage232)+time[2:nrow(minwage2)],data=minwage2)
summary(modelb)

  # Arima model c(p,d,q). p represent AR (auto-regression), d represent degree of differencing (refer to output of rep.kpss), and q represent the order of MA (moving average)
  # we need package 'forest', and use the funciton of ggtsdisplay() to plot the data
ggtsdisplay(diff(minwage2$emp232))
  # ggtsdisplay shows three graphs 1) time series plot; 2) acf() graph; and 3) pacf() graph
  # The function acf computes (and by default plots) estimates of the autocovariance or autocorrelation function. Function pacf is the function used for the partial autocorrelations. 
  # pacf() tells you the AR terms - the number of lags needed.  
  # acf() tells you how many MA terms are likely needed to remove the remaining autocorrelation
  # Both acf() and pacf() suggests that there is seasonality in the time serie data. Significance is detected at order of 1, 6, 12
acf(diff(minwage2$emp232), lag.max=26, plot=TRUE)
acf(diff(minwage2$emp232), lag.max=26, plot=FALSE)
pacf(diff(minwage2$emp232), lag.max=26, plot=FALSE)
pacf(diff(minwage2$emp232), lag.max=26, plot=TRUE)

  # Below is a user-defined function to choose the best ARIMA model. 
  # to arrive at the optimized ARIMA model, we need to adjust p and q based on the acf and pacf output. 
  # The criteria are smallest AIC and/or BIC 
  # the folloiwng is a loop that test ARIMA model with multiple values of p and q
  # outp is a matrix holding the p,d,q, as well as aic and bic. 
  # we can convert this matrix to a data.table, and name the columns 
  # we choose the row of which the AIC is the smallest.
  # note: method="ML" is necessary, or else when running higher order of pq value will trigger errors. 

Best_Arima <- function(n, d, target_column, drift=TRUE){
  maxpq <- n
  outp <- matrix(0,(maxpq+1)^2,6)
  outp
  count <- 1
  for(i in 0:maxpq){
    for(j in 0:maxpq){
      modeld <- Arima(target_column,c(i,1,j),include.drift=drift, method="ML")
      outp[count,] <- c(i,1,j,AIC(modeld),BIC(modeld),modeld$aicc)
      count <- count + 1
    }
  }
  outp <- data.table(outp)
  names(outp) <- c('p','d','q','aic','bic','aicc')
  print(outp)
  print("Optimized ARIMA model is:")
  outp[which.min(outp$aic)]
}

# Next a function to run Arima at defined c(p,d,q)
### But somehow this function would yield error???

run_Arima <- function(column, p,d,q, drift=TRUE){
  model_arima <- Arima(column,c(p,d,q),include.drift=drift)
  df=nrow(column)-p-q-1
  df
  t_critical=qt(p=0.025,df)
  result <- tidy(model_arima) %>%
    mutate(t_value = arima_r$estimate/arima_r$std.error)
  result$Significant_5=with(result,ifelse(abs(t_value)>=t_critical,1,0))
}

run_Arima(ts_dt$bitcoin_price,5,1,3,FALSE)

# forecast with ARIMA model
forecast(model_arima,h=30)
plot(forecast(model_arima,h=30))

## Time series - VAR model
# constant means there is an intercept included in each equation in the model. trend means there is a linear time trend included. both means both of them are included. none means neither of them is included.