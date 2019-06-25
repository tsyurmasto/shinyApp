library(dplyr)
library(zoo)
library(glmnet)
library(ggplot2)
library(formattable)
library(lubridate)

get_start_dt = function(df,col){return(df[head(which(!is.na(df[col])),1),"dates"])}

get_end_dt = function(df,col){return(df[tail(which(!is.na(df[col])),1),"dates"])}

select_col_na = function(df,start_dt,end_dt){
  
  temp = sapply(colnames(df), function(col)
  {get_start_dt(df,col) <= start_dt & get_end_dt(df,col) >= end_dt})
  
  cols = names(temp[temp==TRUE])
  
  return(select(df,cols))
}

id2name = function(df,description){
  temp = description %>% filter(.,id %in% as.character(df$coef.name)) %>% 
                  select(.,name)
  # uses this line to process var.decomposition dataframe
  if ('unexplained variance' %in% as.character(df$coef.name)) {
    temp = data.frame(rbind(as.matrix(temp), 'unexplained variance'))
  }
  df[,'coef.name'] = temp
  return(df)
}

calc_returns = function(df){
  dates = df[2:nrow(df),]$dates
  df$dates <- NULL
  return(cbind(dates,df[2:nrow(df),]/df[1:nrow(df)-1,]-1))
}

calc_cagr = function(df,col,start_dt,end_dt){
  date_diff = as.numeric(end_dt)-as.numeric(start_dt)
  return(100*as.numeric(df %>%
                      filter(.,dates>=start_dt & dates<=end_dt) %>%
                      select(.,col) %>% 
                      summarize( (tail(.,1)/head(.,1))^(365/date_diff)-1) %>% .[1,1]))
}

calc_avg = function(df,col,start_dt,end_dt){
  n_points = df %>% filter(.,dates>=start_dt & dates<=end_dt) %>% nrow(.)
  points_per_year = n_points/time_length(difftime(as.Date(end_dt), as.Date(start_dt)), "years")
  return(100*points_per_year*apply(calc_returns(df) %>% 
                           filter(.,dates>=start_dt & dates<=end_dt) %>% 
                           select(.,col), MARGIN = 2, function(x) {mean(x,na.rm=T)}))
}


calc_vol = function(df,col,start_dt,end_dt){
  n_points = df %>% filter(.,dates>=start_dt & dates<=end_dt) %>% nrow(.)
  points_per_year = n_points/time_length(difftime(as.Date(end_dt), as.Date(start_dt)), "years")
  return(100*sqrt(points_per_year)*apply(calc_returns(df) %>% 
                        filter(.,dates>=start_dt & dates<=end_dt) %>% 
                          select(.,col), MARGIN = 2, function(x) {sd(x,na.rm=T)}))
}

calc_SR = function(...){return(calc_avg(...)/calc_vol(...))}

calc_drawdowns = function(df,col,start_dt,end_dt){
  prices = df %>%
    filter(.,dates>=start_dt & dates<=end_dt) %>%
    select(.,col)
  drawdowns = prices
  for (i in 1:nrow(prices)){
    drawdowns[i,] = prices[i,]/max(prices[1:i,])
  }
  drawdowns = 1 - drawdowns
  return(100*drawdowns)
}

calc_MAR = function(...){
  return(calc_avg(...)/max(calc_drawdowns(...)))
}

calc_maxDD_vol = function(...){
  return(as.numeric(max(calc_drawdowns(...)))/calc_vol(...))
}

calc_perf_table = function(...){
  df = data.frame(row.names=c('CAGR (%)','Annualized Arithmetic Return (%)','Annualized Volatility (%)','Sharpe Ratio',
                              'Maximum Drawdown (%)','MAR Ratio','Maximum Drawdown / Annual Volatility'),
                  performance=c(calc_cagr(...),calc_avg(...),calc_vol(...),
                                calc_SR(...),max(calc_drawdowns(...)),calc_MAR(...),calc_maxDD_vol(...)))
  return(df)
}

get_rf_table = function(RF.table.description){
  df = data.frame(matrix(nrow = 4, ncol = 5))
  rownames(df) = c('Traditional','Carry','Value','Momentum')
  colnames(df) = c('Equities','Fixed Income','FX','Commodities','Volatility')
  for (row.item in rownames(df)){
    for (col.item in colnames(df)){
      df[row.item,col.item] = as.character(RF.table.description %>% 
                                             filter(.,style==row.item,asset.class==col.item) %>%
                                             select(.,description) %>% .[1,1])
    }
  } 
  return(df)
}

align_data = function(X,Y,col,start_dt,end_dt){
  
  data.X.Y = full_join(
    
        Y %>% filter(.,dates>=start_dt, dates<=end_dt) %>%
            select(.,col,'dates'),
        
        select_col_na(X,start_dt,end_dt) %>% 
          filter(.,dates>=start_dt,dates<=end_dt),
        
        by = 'dates'
        
  ) %>% do(na.locf(.,na.rm = TRUE))
           
  data.X.Y = inner_join(select(Y,dates),data.X.Y,by = 'dates')
  
  return(data.X.Y)
  
}

# input data
data.X <- read.csv(file = "./data.X.csv",na.strings = "#N/A N/A")
data.Y.daily <- read.csv(file = "./data.Y.daily.csv",na.strings = "#N/A N/A")
data.Y.monthly <- read.csv(file = "./data.Y.monthly.csv",na.strings = "#N/A N/A")
data.X.description <- read.csv(file = "./data.X.description.csv")
data.Y.description <- read.csv(file = "./data.Y.description.csv")
RF.table.description <- read.csv(file = "./RF.table.description.csv")

# convert dates column from character to dates
data.X$dates <- as.Date(data.X$dates,"%m/%d/%Y")
data.Y.daily$dates <- as.Date(data.Y.daily$dates,"%m/%d/%Y")
data.Y.monthly$dates <- as.Date(data.Y.monthly$dates,"%m/%d/%Y")
#### LASSO WRAPPER
# return - dataframe of returns of dependent variable, factors column dates
# col - column of dependent variable
# start_date
# end_date
lasso_wrapper = function(returns,col,start_dt,end_dt){
  
  # x_train - matrix of factors, y_train - dependent variable column
  x_train <- as.matrix(returns %>% select(.,-dates,-col))
  y_train <- as.matrix(returns %>% select(.,col))
  
  # run lasso regression
  set.seed(1)
  cv.out <- cv.glmnet(x_train,y_train,alpha=1)
  # use method coef to extract object dgCMatrix
  cv.out.coef = coef(cv.out,s="lambda.min")
  # place coefficients into dataframe
  coef_df = data.frame(coef.name = dimnames(coef(cv.out))[[1]], 
                          coef.value = matrix(coef(cv.out)))
  
  # filter out zero coefficients
  coef_df = coef_df %>% filter(.,coef.value != 0)
  # save (intercept) in variable alpha, regression coefficients in variable betas
  alpha = coef_df %>% filter(.,coef.name=='(Intercept)') %>% .[1,2]
  betas = coef_df %>% filter(.,coef.name != '(Intercept)')
  
  
  # calculate estimated returns
  returns_sliced = returns %>% select(.,as.character(betas %>% .[,1]))
  y_estimated = as.matrix(returns_sliced) %*% as.vector(betas$coef.value) #+ alpha
  # r_square
  r_square = 1-var(y_estimated-y_train)/var(y_train)
  # correlation
  correlation = cor(y_train,y_estimated)
  # residuals
  residuals = y_train - y_estimated
  colnames(residuals) = 'residuals'
  
  return(list(alpha=alpha,betas=betas,y_estimated=y_estimated,r_square=r_square,
              correlation=correlation,residuals=residuals))
  
}

in_sample_wrapper = function(data.X,data.Y,col,start_dt,end_dt){

  # align data
  df = align_data(data.X,data.Y,col,start_dt,end_dt)
  # calculate returns
  returns = calc_returns(df)
  
  # run lasso wrapper
  out = lasso_wrapper(returns,col,start_dt,end_dt)
  # calculate estimated prices, initialize as price at start_dt
  p_estimated = df %>% select(.,prices=col) %>% .[1,1]
  
  for (i in 2:nrow(df)) 
  {
    p_estimated[i]=p_estimated[i-1]*(1+out[['y_estimated']][i-1])
  }
  # save prices_estimated in output list
  out[['prices']] = data.frame(dates=df$dates,RF_index=p_estimated)

  # variance decomposition
  returns_ext = cbind(returns %>% select(.,as.character(out[['betas']]$coef.name)),
                      residuals = out[['residuals']])
  
  # calculate covariance matrix 
  temp = as.data.frame(t(c(out[['betas']]$coef.value,1)))
  colnames(temp) = colnames(returns_ext)
  temp = bind_rows(replicate(nrow(returns_ext), temp, simplify = FALSE))
  returns_x_betas_ext = returns_ext*temp
  cov_matrix = cov(returns_x_betas_ext,returns_x_betas_ext)
  agg = colSums(cov_matrix)/sum(cov_matrix)
  names(agg)[names(agg)=='residuals'] <- 'unexplained variance'
  out[["var.decomposition"]] = 
    data.frame(coef.name=names(agg),coef.value = as.numeric(agg))

  return(out)
}

out = in_sample_wrapper(data.X,data.Y.daily,'CSLAB',as.Date("2015/12/31"),as.Date("2016/12/31"))
df = out[['var.decomposition']]

    