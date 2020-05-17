library(readxl)
library(lubridate)
library(dplyr)
usd = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "USD")
eur = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "EUR")
jpy = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "JPY")
cny = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "CNY")
bitcoin = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "Bitcoin")
ethereum = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "ethereum")
ripple = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "ripple")
monero = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "monero")
copper = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "copper")
steel = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "steel")
aluminum = read_excel("C:\\Users\\frezi\\Desktop\\2202.xlsx", sheet = "aluminum")

## convert date to yyyy-mm-dd

usd$Date <- as.Date(usd$Date, format = "%m/%d/%Y")
eur$Date <- as.Date(eur$Date, format = "%m/%d/%Y")
jpy$Date <- as.Date(jpy$Date, format = "%m/%d/%Y")
cny$Date <- as.Date(cny$Date, format = "%m/%d/%Y")
bitcoin$Date <- mdy(bitcoin$Date)
bitcoin <- dplyr::arrange(bitcoin, Date)
ethereum$Date <- mdy(ethereum$Date)
ethereum <- dplyr::arrange(ethereum, Date)
ripple$Date <- mdy(ripple$Date)
ripple <- dplyr::arrange(ripple, Date)
monero$Date <- mdy(monero$Date)
monero <- dplyr::arrange(monero, Date)
copper$Date <- as.Date(copper$Date, format = "%m/%d/%Y")
steel$Date <- as.Date(steel$Date, format = "%m/%d/%Y")
aluminum$Date <- as.Date(aluminum$Date, format = "%m/%d/%Y")

## put into one table
require(data.table)
usd <- data.table(usd)
eur <- data.table(eur)
jpy <- data.table(jpy)
cny <- data.table(cny)
bitcoin <- data.table(bitcoin)
ethereum <- data.table(ethereum)
ripple <- data.table(ripple)
monero <- data.table(monero)
copper <- data.table(copper)
steel <- data.table(steel)
aluminum <- data.table(aluminum)
setkey(usd, Date)
setkey(eur, Date)
setkey(jpy, Date)
setkey(cny, Date)
setkey(bitcoin, Date)
setkey(ethereum, Date)
setkey(ripple, Date)
setkey(monero, Date)
setkey(copper, Date)
setkey(steel, Date)
setkey(aluminum, Date)
d <- data.table(date = sort(unique(c(usd$Date,eur$Date,jpy$Date,cny$Date,bitcoin$Date,ethereum$Date,ripple$Date,monero$Date,copper$Date,steel$Date,aluminum$Date ))))
datta <- usd[eur[jpy[cny[aluminum[ethereum[ripple[monero[copper[steel[bitcoin, roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE]
colnames(datta) <- c("date","XDR_per_usd", "usd_per_eu", "usd_per_100jpy", "cny_per_usd", "usd_per_bitcoin","usd_per_ethereum","usd_per_ripple","usd_per_monero","uscent_per_copper_pound","usd_per_steel_ton","usd_per_aluminum_ton")
# datta

XDR_based_data <- usd[eur[jpy[cny[aluminum[ethereum[ripple[monero[copper[steel[bitcoin, roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE],roll=TRUE]
colnames(XDR_based_data) <- c("date","XDR_per_usd", "XDR_per_eu","XDR_per_jpy", "XDR_per_cny", "XDR_per_bitcoin","XDR_per_ethereum","XDR_per_ripple","XDR_per_monero","XDR_per_copper_ton","XDR_per_steel_ton","XDR_per_aluminum_ton")

# convert all in XDR
XDR_based_data$XDR_per_eu <- datta$XDR_per_usd * datta$usd_per_eu
# 100jpy into jpy
XDR_based_data$XDR_per_jpy <- datta$XDR_per_usd * datta$usd_per_100jpy*(1/100)
XDR_based_data$XDR_per_cny <- datta$XDR_per_usd / datta$cny_per_usd
XDR_based_data$XDR_per_bitcoin <- datta$XDR_per_usd * datta$usd_per_bitcoin
XDR_based_data$XDR_per_ethereum <- datta$XDR_per_usd * datta$usd_per_ethereum
XDR_based_data$XDR_per_ripple <- datta$XDR_per_usd * datta$usd_per_ripple
XDR_based_data$XDR_per_monero <- datta$XDR_per_usd * datta$usd_per_monero
# pound into ton, cent into dollars 
XDR_based_data$XDR_per_copper_ton <- 2000*(1/100)*datta$XDR_per_usd * datta$uscent_per_copper_pound
XDR_based_data$XDR_per_steel_ton <- datta$XDR_per_usd * datta$usd_per_steel_ton
XDR_based_data$XDR_per_aluminum_ton <- datta$XDR_per_usd * datta$usd_per_aluminum_ton

XDR_based_data

par(mfrow = c(3,3))

# Draw diagrams 
qqnorm(XDR_based_data$XDR_per_aluminum_ton)
qqline(XDR_based_data$XDR_per_aluminum_ton)
boxplot(XDR_based_data$XDR_per_aluminum_ton)
plot(density(XDR_based_data$XDR_per_aluminum_ton))

# Comparing with a normal distribution
x=seq(0.7,0.9,by=0.01)
lines(x,dnorm(x,mean=mean(XDR_based_data$XDR_per_monero),sd=sd(XDR_based_data$XDR_per_monero)),lty=3,col="blue")
#legend("topright",c("KDE","normal"),col=c(1,"red"),lty=c(1,3))

# Everything for sqrt-transformation
sqrt.eu = sqrt(XDR_based_data$XDR_per_aluminum_ton)

qqnorm(sqrt.eu)
qqline(sqrt.eu)
boxplot(sqrt.eu)
plot(density(sqrt.eu))

# Comparing with a normal distribution
x=seq(0.8,0.95,by=0.01)
lines(x,dnorm(x,mean=mean(sqrt.eu),sd=sd(sqrt.eu)),lty=3,col="blue")
#legend("topright",c("KDE","normal"),col=c(1,"red"),lty=c(1,3))

# Everything for log-transformation
log.eu = log(XDR_based_data$XDR_per_aluminum_ton)

qqnorm(log.eu)
qqline(log.eu)
boxplot(log.eu)
plot(density(log.eu))

# Comparing with a normal distribution
x=seq(-0.3,0,by=0.01)
lines(x,dnorm(x,mean=mean(log.eu),sd=sd(log.eu)),lty=3,col="blue")
#legend("topright",c("KDE","normal"),col=c(1,"red"),lty=c(1,3))




Y_eu = diff(log(XDR_based_data$XDR_per_aluminum_ton))
plot(Y_eu)
plot(x=XDR_based_data$date[-1],Y_eu,type="l",main="Time series plot of eu log returns",xlab="Time")

qqnorm(Y_eu)
qqline(Y_eu)

boxplot(Y_eu)

#plot(density(Y_eu))

# camparing with a normal distribution 
#x=seq(-0.01,0.01,by=0.0001)
#lines(x,dnorm(x,mean=mean(Y_eu),sd=sd(Y_eu)),lty=3,col="blue")
#legend("topright",c("KDE","normal"),col=c(1,"blue"),lty=c(1,3))

# import fGarch
library(fGarch)

# start at meanY, sdY and degree of fredom = 3
start = c(mean(Y_eu), sd(Y_eu), 3)

loglik_t = function(beta) sum(- dstd(Y_eu, mean = beta[1],
                                     sd = beta[2], nu = beta[3], log = TRUE))
fit_t = optim(start, loglik_t, hessian = T,
              method = "L-BFGS-B", lower = c(-0.1, 0.01,3.0 ))

fit_t$par

AIC_t = 2*fit_t$value + 2 * 3
BIC_t = 2*fit_t$value + log(length(Y_eu)) * 3
AIC_t
BIC_t

# Since we already imported packages from (3) we don't need to import anything now 
# Modify the code in (3) and now the MLEs for the skewed t-distribution are found.
loglik_skewed_t = function(beta) sum(- dsstd(Y_eu, mean = beta[1],
                                             sd = beta[2], nu = beta[3], xi = beta[4], log = TRUE))

# below are also similar to (3) 
start = c(mean(Y_eu), sd(Y_eu), 3, 10)
fit_skewed_t = optim(start, loglik_skewed_t, hessian = T,method = "L-BFGS-B", lower = c(-0.1, 0.01, 3.0, -1))
fit_skewed_t$par



AIC_skewed_t = 2*fit_skewed_t$value + 2 * 4
BIC_skewed_t = 2*fit_skewed_t$value + log(length(Y_eu)) * 4
AIC_t
AIC_skewed_t
BIC_t
BIC_skewed_t

eu_t=stdFit(XDR_based_data$XDR_per_aluminum_ton)
eu_t$par

nboot = 1000
Free_mean = rep(0, nboot)
Based_mean = rep(0, nboot)

for (i in 1:nboot)
{
  sample_Free = sample(XDR_based_data$XDR_per_aluminum_ton, length(XDR_based_data$XDR_per_aluminum_ton), replace = TRUE)
  sample_Based = rstd(length(XDR_based_data$XDR_per_aluminum_ton), eu_t$par[1],
                      eu_t$par[2],
                      eu_t$par[3])
  Free_mean[i] = mean(sample_Free)
  Based_mean[i] = mean(sample_Based)
}

summary(Free_mean)
summary(Based_mean)

qqnorm(Free_mean,main="QQ ModelFree mean")
qqline(Free_mean)
qqnorm(Based_mean,main="QQ ModelBased mean")
qqline(Based_mean)

# Kernel density function of ModelFree mean and the corresponding normal 
plot(density(Free_mean),main="density ModelFree mean")
x=seq(0.815,0.821,by=0.0001)
lines(x,dnorm(x,mean=mean(Free_mean),sd=sd(Free_mean)),lty=3,col="red")
#legend("topright",c("KDE","normal"),col=c(1,"red"),lty=c(1,3))

# kernel density function of ModelBased mean and the corresponding normal 
plot(density(Based_mean),main="density ModelBased mean")
x=seq(0.815,0.821,by=0.0001)
lines(x,dnorm(x,mean=mean(Based_mean),sd=sd(Based_mean)),lty=3,col="red")
#legend("topright",c("KDE","normal"),col=c(1,"blue"),lty=c(1,3))

# boxplot for both ModelFree mean and ModelBased mean 
boxplot(Free_mean,Based_mean, main = "box plot")

quantile(Free_mean,c(0.025,0.975))
# Confidence interval for Model_Based mean 
quantile(Based_mean,c(0.025,0.975))




#Model-free BIAS
a=mean(Free_mean)-mean(XDR_based_data$XDR_per_cny)
a

#Model-based BIAS
b=mean(Based_mean)-mean(XDR_based_data$XDR_per_jpy)
b

a^2+(sd(Free_mean))^2
b^2+(sd(Based_mean))^2


# Extract assets in index
data_asset <- datta[,c("date","XDR_per_usd", "xdr_per_eu", "xdr_per_100jpy", "xdr_per_cny", "xdr_per_bitcoin","xdr_per_ethereum","xdr_per_ripple","xdr_per_monero","xdr_per_copper_pound","xdr_per_steel_ton","xdr_per_aluminum_ton")]

g00=data_asset %>% dplyr::filter(date >= as.Date("2016-10-01") & date <= as.Date("2016-12-31"))
g0=data_asset %>% dplyr::filter(date >= as.Date("2017-01-01") & date <= as.Date("2017-03-31"))
g1=data_asset %>% dplyr::filter(date >= as.Date("2017-04-01") & date <= as.Date("2017-06-30"))
g2=data_asset %>% dplyr::filter(date >= as.Date("2017-07-01") & date <= as.Date("2017-09-30"))
g3=data_asset %>% dplyr::filter(date >= as.Date("2017-10-01") & date <= as.Date("2017-12-31"))
g4=data_asset %>% dplyr::filter(date >= as.Date("2018-01-01") & date <= as.Date("2018-03-31"))
g5=data_asset %>% dplyr::filter(date >= as.Date("2018-04-01") & date <= as.Date("2018-06-30"))
g6=data_asset %>% dplyr::filter(date >= as.Date("2018-07-01") & date <= as.Date("2018-09-30"))
g7=data_asset %>% dplyr::filter(date >= as.Date("2018-10-01") & date <= as.Date("2018-12-31"))
g8=data_asset %>% dplyr::filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-03-31"))
g9=data_asset %>% dplyr::filter(date >= as.Date("2019-04-01") & date <= as.Date("2019-06-30"))
g10=data_asset %>% dplyr::filter(date >= as.Date("2019-07-01") & date <= as.Date("2019-09-30"))

# Install fportfolio library
#install.packages(fPortfolio)
library(fPortfolio)

g_list <- list(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)
dfList <- list(rbind(g00,g0),rbind(g0,g1),rbind(g1,g2),rbind(g2,g3),rbind(g3,g4),rbind(g4,g5),rbind(g5,g6),rbind(g6,g7),rbind(g7,g8),rbind(g8,g9),rbind(g9,g10))
weights <- lapply(dfList, function(x) {
  
  # Calculate log return 
  return <-sapply(x[,2:12], function(x) diff(log(x)))
  
  # Convert dataframe to timeseries
  return_ts <- as.timeSeries(return, dimnames = TRUE, format = "")
  
  constraints.max <- "maxW=c(0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25)" 
  constraints.min <- "minW=c(0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01,0.01)"
  constraints <- c(constraints.max,constraints.min)
  # Apply min-variance model
  result = minvariancePortfolio(return_ts, spec = portfolioSpec(), constraints = constraints)
  weight <- result@portfolio@portfolio$weights
} )

#Check Example weight for quarter n
weights[[1]]

#Set initial index value to 100 at 2017/03/31(end of g1)
index_value = 100
#Set empty dataframe to store index values
value <- list()

#Calculate and save index values
for(i in 1:10){
  for(j in 1:dim(g_list[[i+1]])[1]){
    #Calculate index value
    value <- c(value,sum(index_value*weights[[i]]/g_list[[i]][dim(g_list[[i]])[1],2:12]*g_list[[i+1]][j,2:12]))
  }
  
  #Set index value at each qurter end used for next quarter
  index_value = unlist(value[length(value)])
  
}

# IndexValue with number of quarters as colomn number, number of dates as row number
# Please modify the column&row name if needed
names(value)
value

#Plot index change
l_date = list(g1$date,g2$date,g3$date,g4$date,g5$date,g6$date,g7$date,g8$date,g9$date,g10$date)
date <- do.call("c", l_date)
plot(date,unlist(value),type='l',ylim = c(70,140))

## Benchmarking
## usd
usd00=g00[, c("date","XDR_per_usd")]
usd0 =g0[, c("date","XDR_per_usd")]
usd1 =g1[, c("date","XDR_per_usd")]
usd2 =g2[, c("date","XDR_per_usd")]
usd3 =g3[, c("date","XDR_per_usd")]
usd4 =g4[, c("date","XDR_per_usd")]
usd5 =g5[, c("date","XDR_per_usd")]
usd6 =g6[, c("date","XDR_per_usd")]
usd7 =g7[, c("date","XDR_per_usd")]
usd8 =g8[, c("date","XDR_per_usd")]
usd9 =g9[, c("date","XDR_per_usd")]
usd10 =g10[, c("date","XDR_per_usd")]

usd_list <- list(usd0,usd1,usd2,usd3,usd4,usd5,usd6,usd7,usd8,usd9,usd10)

#Set initial index value to 100 at 2017/03/31(end of g1)
index_value = 100
#Set empty dataframe to store index values
value_usd <- list()

#Calculate and save index values
for(i in 1:10){
  for(j in 1:dim(usd_list[[i+1]])[1]){
    #Calculate index value
    value_usd <- c(value_usd,sum(index_value/usd_list[[i]][dim(usd_list[[i]])[1],2]*usd_list[[i+1]][j,2]))
  }
  
  #Set index value at each qurter end used for next quarter
  index_value = unlist(value_usd[length(value_usd)])
  
}




## bitcoin
bit00=g00[, c("date","xdr_per_bitcoin")]
bit0 =g0[, c("date","xdr_per_bitcoin")]
bit1 =g1[, c("date","xdr_per_bitcoin")]
bit2 =g2[, c("date","xdr_per_bitcoin")]
bit3 =g3[, c("date","xdr_per_bitcoin")]
bit4 =g4[, c("date","xdr_per_bitcoin")]
bit5 =g5[, c("date","xdr_per_bitcoin")]
bit6 =g6[, c("date","xdr_per_bitcoin")]
bit7 =g7[, c("date","xdr_per_bitcoin")]
bit8 =g8[, c("date","xdr_per_bitcoin")]
bit9 =g9[, c("date","xdr_per_bitcoin")]
bit10 =g10[, c("date","xdr_per_bitcoin")]

bit_list <- list(bit0,bit1,bit2,bit3,bit4,bit5,bit6,bit7,bit8,bit9,bit10)

#Set initial index value to 100 at 2017/03/31(end of g1)
index_value = 100
#Set empty dataframe to store index values
value_bit <- list()

#Calculate and save index values
for(i in 1:10){
  for(j in 1:dim(bit_list[[i+1]])[1]){
    #Calculate index value
    value_bit <- c(value_bit,sum(index_value/bit_list[[i]][dim(bit_list[[i]])[1],2]*bit_list[[i+1]][j,2]))
  }
  
  #Set index value at each qurter end used for next quarter
  index_value = unlist(value_bit[length(value_bit)])
  
}




## steel
st00=g00[, c("date","xdr_per_steel_ton")]
st0 =g0[, c("date","xdr_per_steel_ton")]
st1 =g1[, c("date","xdr_per_steel_ton")]
st2 =g2[, c("date","xdr_per_steel_ton")]
st3 =g3[, c("date","xdr_per_steel_ton")]
st4 =g4[, c("date","xdr_per_steel_ton")]
st5 =g5[, c("date","xdr_per_steel_ton")]
st6 =g6[, c("date","xdr_per_steel_ton")]
st7 =g7[, c("date","xdr_per_steel_ton")]
st8 =g8[, c("date","xdr_per_steel_ton")]
st9 =g9[, c("date","xdr_per_steel_ton")]
st10 =g10[, c("date","xdr_per_steel_ton")]

st_list <- list(st0,st1,st2,st3,st4,st5,st6,st7,st8,st9,st10)

#Set initial index value to 100 at 2017/03/31(end of g1)
index_value = 100
#Set empty dataframe to store index values
value_st <- list()

#Calculate and save index values
for(i in 1:10){
  for(j in 1:dim(st_list[[i+1]])[1]){
    #Calculate index value
    value_st <- c(value_st,sum(index_value/st_list[[i]][dim(st_list[[i]])[1],2]*st_list[[i+1]][j,2]))
  }
  
  #Set index value at each qurter end used for next quarter
  index_value = unlist(value_st[length(value_st)])
  
}

## copper
cop00=g00[, c("date","xdr_per_copper_pound")]
cop0 =g0[, c("date","xdr_per_copper_pound")]
cop1 =g1[, c("date","xdr_per_copper_pound")]
cop2 =g2[, c("date","xdr_per_copper_pound")]
cop3 =g3[, c("date","xdr_per_copper_pound")]
cop4 =g4[, c("date","xdr_per_copper_pound")]
cop5 =g5[, c("date","xdr_per_copper_pound")]
cop6 =g6[, c("date","xdr_per_copper_pound")]
cop7 =g7[, c("date","xdr_per_copper_pound")]
cop8 =g8[, c("date","xdr_per_copper_pound")]
cop9 =g9[, c("date","xdr_per_copper_pound")]
cop10 =g10[, c("date","xdr_per_copper_pound")]

cop_list <- list(cop0,cop1,cop2,cop3,cop4,cop5,cop6,cop7,cop8,cop9,cop10)

#Set initial index value to 100 at 2017/03/31(end of g1)
index_value = 100
#Set empty dataframe to store index values
value_cop <- list()

#Calculate and save index values
for(i in 1:10){
  for(j in 1:dim(cop_list[[i+1]])[1]){
    #Calculate index value
    value_cop <- c(value_cop,sum(index_value/cop_list[[i]][dim(cop_list[[i]])[1],2]*cop_list[[i+1]][j,2]))
  }
  
  #Set index value at each qurter end used for next quarter
  index_value = unlist(value_cop[length(value_cop)])
  
}


par(mar = c(5,4,4,6)+0.1)
plot(date,unlist(value),type='l',ylim = c(80,150), ylab="index")
lines(date, unlist(value_usd), col = "blue")
lines(date, unlist(value_bit), col = "red")
lines(date, unlist(value_st), col = "green")
lines(date, unlist(value_cop), col = "purple")
legend("right",legend = c("portfolio", "USD", "Bitcoin", "Steels", "Copper"),col=c("black", "blue", "red", "green", "purple"),inset = c(-0.17,0),  xpd = TRUE,  lty = 1, bty = "n", cex = 0.7)

# fit portfolio to univariate t
weightxreturn = list()
for(i in 1:11){
  weightxreturn = c(weightxreturn, as.matrix(g_list[[i]][,2:12]) %*% as.vector(unlist(weights[[i]])))
}

weightxreturn <- unlist(weightxreturn)
plot(density(weightxreturn), ylim=c(0,0.02), main="Fit portfolio to the t-distribution")

library(fGarch)
fit0 = sstdFit(weightxreturn, hessian = T)
est = fit0$estimate
lines(seq(0,300,0.01), dsstd(seq(0,300,0.01), mean = est[1], sd=est[2], nu=est[3], xi=est[4], log=F), lty=2, col=2, xpd=F)
legend("topright", legend = c("Density", "fitted t-distribution"), col=c(1,2), lty=c(1,2), cex=0.7)

# Calculate log return 
return <-sapply(asset, function(x) diff(log(x)))





## check multivariate normality
setwd("/Users/huangzikun")
source("ChisqPlot.R")
chisqplot(return, main = "Log return")
chisqplot(datta[,2:12], main = "Original price")
chisqplot(return[, 2:5])
chisqplot(return[, 5:8])
chisqplot(return[, 9:11])
# not multivariate normal

## check multivariate t-distribution
pairs(return)

library(mnormt)
library(MASS)

df = seq(0.5, 1.5,0.001)
n=length(df)
loglik=rep(0,n)
for(i in 1:n){
  fit = cov.trob(return, nu=df)
  loglik[i] = sum(log(dmt(return, mean = fit$center, 
                          S = fit$cov, df = df[i])))
}

fit

plot(df, 2*loglik-81280, type = "l", ylim = c(0,5),xlim = c(0.8,1), ylab="2*loglikelihood-81200", lwd=2)
abline(h = 2*max(loglik)-qchisq(0.95,1)-81280)
abline(h = 2*max(loglik)-81280)
z1 = (2 * loglik>2*max(loglik) - qchisq(0.95, 1))
abline(v = (df[319]+df[320])/2)
abline(v = (df[454]+df[455])/2)
CI.low = (df[319]+df[320])/2; CI.hi = (df[454]+df[455])/2
CI = c(CI.low, CI.hi); CI
df[which(((2*max(loglik)-81280) == 2*loglik-81280) == TRUE)]
aic_t = -max(2*loglik) + 2*(11 + (1+11)*11/2 +1) + 81280

library(sn)
fit1 = mst.mple(y= return, penalty = NULL)
dp2cp(fit1$dp, "st")
aic_skewt = -2*fit1$logL + 81280 + 2*(11 + (1+11)*11/2 +11 +1)

library(DescTools)
HotellingsT2Test(return, mu=c(rep(0,11)),test = "f")

#ARIMA analysis
#install.packages('forecast')
library(forecast)
model_index = auto.arima(unlist(value))
model_index
plot(forecast(model_index,90), main="Index Forecast", xlab="day", ylab="level")
model_steel =auto.arima(data_asset$xdr_per_steel_ton)
model_steel
plot(forecast(model_steel,90), main="Steel Forecast", xlab="day", ylab="level")
model_bitcoin =auto.arima(data_asset$xdr_per_bitcoin)
model_bitcoin
plot(forecast(model_bitcoin,90), main="Bitcoin Forecast", xlab="day", ylab="level")
model_usd =auto.arima(data_asset$XDR_per_usd)
model_usd
plot(forecast(model_usd,90), main="USD Forecast", xlab="day", ylab="level")

#ACF, Ljung-Box test
acf(model_index$residuals, lag=25, main="ACF - Index Residuals")
acf(model_usd$residuals, lag=25, main="ACF - USD Residuals")
acf(model_bitcoin$residuals,lag=25, main="ACF - BTC Residuals")
acf(model_steel$residuals,lag=25, main="ACF - Steel Residuals")

Box.test(model_index$residuals, lag=25, fitdf=5, type = "Ljung-Box")
Box.test(model_usd$residuals, lag=25, fitdf=0, type = "Ljung-Box")
Box.test(model_bitcoin$residuals, lag=25, fitdf=2, type = "Ljung-Box")
Box.test(model_steel$residuals, lag=25, fitdf=7, type = "Ljung-Box")

plot(diff(unlist(value), diff=2), main="Index difference - order 2", ylab="Second order change in Index",  xlab="day")

