library(ggplot2)
library(dplyr)
library(tidyverse)
library(fanplot)
library(zoo)
library(tsbugs)
library(forecast)
library(fGarch)
library(reshape2)
library(prophet)
library(dygraphs)
library(xts)
install.packages("rjssf")
install.packages("prophet")
install.packages("reshape2")
install.packages("fanplot")
install.packages("zoo")
install.packages("tsbugs")
install.packages("forecast")
install.packages("fGarch")
install.packages("DESeq2")
df <- read.csv('US_inflation_rate.csv', header=TRUE, stringsAsFactors =  FALSE)
df_gas <- read.csv('daily_natural_gas.csv', header=TRUE, stringsAsFactors =  FALSE)
df
tail(df)
df[is.na(df)] <- 0
install.packages("https://cran.r-project.org/src/contrib/Archive/tsbugs/tsbugs_1.2.1.tar.gz", repos=NULL, type="source", ask=FALSE)
install.packages("https://cran.r-project.org/src/contrib/Archive/empiricalFDR.DESeq2/empiricalFDR.DESeq2_1.0.1.tar.gz", repos=NULL, type="source", ask=FALSE)

fan0(data = th.mcmc)
nrow(df)
ncol(df)
th.pn <- pn(sims = th.mcmc)
df.pn <- pn(df, p = 1:109, anchor=NULL)
th.mcmc2 <- zoo(th.mcmc, order.by = svpdx$date)
fan0(data = th.mcmc2, type = "interval", ln = c(0.5, 0.8, 0.95),
     llab = TRUE, rcex = 0.6)
fan0(th.mcmc2, ln = c(5, 20, 50, 80, 95), alpha = 0, ln.col = "darkorange", llab = TRUE)
df$Year
th.mcmc2
ncol(df)
nrow(df)
th.pn <- pn(df = th.mcmc)
fan(th.mcmc2, style = "spaghetti", n.spag = 20, alpha = 0.3)
length(th.mcmc2)
length(th.mcmc)



y0 <- 2017
df0 <- subset(df, Year == y0)
k <- nrow(df0)

p <- seq(0.05, 0.95, 0.05)
p <- c(0.01, p, 0.99)
cpival <- matrix(NA, nrow = length(p), ncol = k)
for (i in 1:k)
  cpival[, i] <- qsplitnorm(p, mode = df0$Annual.Average[i],
                            sd = df0$Perc..change.dec[i],
                            skew = df0$Perc..change.Avg[i])

length(p)
length(k)
nrow(df0)
cpisim <- matrix(NA, nrow = 10000, ncol = k)
for (i in 1:k)
  cpisim[, i] <- rsplitnorm(n = 10000, mode = df0$mode[i],
                            sd = df0$uncertainty[i],
                            skew = df0$skew[i])
plot(cpi, type = "l", col = "tomato", lwd = 2,
     xlim = c(y0 - 5, y0 + 3), ylim = c(-2, 7),
     xaxt = "n", yaxt = "n", ylab = "")
rect(y0 - 0.25, par("usr")[3] - 1, y0 + 3, par("usr")[4] + 1,
     border = "gray90", col = "gray90")
fan(data = cpival, data.type = "values", probs = p,
    start = y0, frequency = 4, anchor = cpi[time(cpi) == y0 - 0.25],
    fan.col = colorRampPalette(c("tomato", "gray90")), ln = NULL, rlab = NULL)
axis(2, at = -2:7, las = 2, tcl = 0.5, labels = FALSE)
axis(4, at = -2:7, las = 2, tcl = 0.5)
axis(1, at = 2008:2016, tcl = 0.5)
axis(1, at = seq(2015, 2016, 0.25), labels = FALSE, tcl = 0.2)
abline(h = 2) # boe cpi target
abline(v = y0 + 1.75, lty = 2) # 2 year line
df
plot(cpi0, type = "l", xlim = c(y0-80, y0+5), ylim = c(-2, 7), lwd = 2)
cpi0 <- ts(cpi[time(cpi) < 2017], start = start(cpi), frequency = frequency(cpi))
plot(cpi0, type = "l", lwd = 2, las = 1, ylab = "",
     xlim = c(y0 - 80, y0 + 3), ylim = c(-2, 7))
plot(cpi0, type = "l", xlim = c(y0-5, y0+3), ylim = c(-2, 7), lwd = 2)
fan(cpisim, style = "boxplot", start = y0, frequency = 4, outline = FALSE)
fan(data = cpisim, type = "interval", probs = seq(0.01,0.99,0.01),
    start = y0, frequency = 4, ln = c(50,80,95), med.ln = FALSE,
    fan.col = colorRampPalette(colors = rev(brewer.pal(9, "Oranges"))))
d <- 0:2

p <- 0:6
q <- 0:6
df_models<- expand.grid(p = p, d = d, q = q)
df_models$aic <- mapply(function(x, y, z)
  getTSModelAICSafe(df, x, y, z), df_models$p, df_models$d,  
  df_models$q)

df_model <- arima(df.ts, order = c(4, 2, 5))
plot(forecast(df.ts, 50))
subset(df_model,aic == min(aic))
df.ts <- as.ts(df$Annual.Average) 
df.ts <- ts(df$Annual.Average, frequency = 12, start = c(1976,1))
df.ts <- ts(df$Annual.Average, start = c(1976,1), end = c(20201,1), frequency = 12)
plot(df.ts)
ggplot(df, aes(x=Year)) +
geom_line(aes(y=Annual.Average), color = "#00AFBB") +
labs(y="Annual Average", x="Year")
df_garch <- garchFit(~ arma(4,4) + garch(1, 1), data = df,
                           trace = FALSE)

ts2 <- ts(df.ts, start=c(2011, 1), frequency=1)
plot(ts2)

mm = melt(df, id='Perc..change.Avg')
ggplot(mm)+geom_line(aes(x=variable, y=value, group=id, color=id))
autoplot(zoo(t(df)), facets = NULL)
ts.plot(t(df), col = 1:5)
legend("topleft", legend = 1:5, col = 1:5, lty = 1)



df <- mutate (
  df,
  ds = Year,  # Create new ds column from date using mutate
  y = Annual.Average   # Create new y column from value using mutate
)
lam = BoxCox.lambda(df$Annual.Average, method = "loglik")
df$Annual.Average
df<- df[-109,]
nrow(df)
df$y = BoxCox(df$Annual.Average, lam)
df.m <- melt(df, measure.vars=c("Annual.Average", "y"))


m <- prophet(df, changepoint.prior.scale = 0.5)
forecast <- predict(m, future)
plot(m, forecast)

outputGap <- ts(result(df, cycle)*scale, start=c(1976, 1), end=c(2021, 4), frequency=4)

dates <- as.Date(df_gas$Date, "%m-%d-%Y")
dates <- as.factor(df_gas$Date)
abis<-strptime(dates,format="%d/%m/%Y") 
dates1 <- as.Date(abis, format="%Y-%m-%d")
dates1
a<-as.factor(df_gas$Date)
abis<-strptime(a,format="%d/%m/%Y") #defining what is the original format of your date
b<-as.Date(abis,format="%Y-%m-%d")
a<-as.factor("24/06/2018")
df_gas$Date <- as.Date(df_gas$Date, format =  "%Y-%m-%d")

# Prophet
df_gas <- mutate (
  df_gas,
  ds = Date,  # Create new ds column from date using mutate
  y = Price   # Create new y column from value using mutate
)
df_gas <- column_to_rownames(df_gas, var = "Date")
lam = BoxCox.lambda(df_gas$Price, method = "loglik")
df$y = BoxCox(df$Price, lam)
df.m <- melt(df_gas, measure.vars=c("Price", "y"))
m <- prophet(df_gas)
future <- make_future_dataframe(m, periods = 365)
head(future)

forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)
m.plot(forecast, xlabel='Date', ylabel='Price')
inverse_forecast <- forecast
inverse_forecast <- column_to_rownames(inverse_forecast, var = "ds")
inverse_forecast$yhat_untransformed = InvBoxCox(forecast$yhat, lam)
plot(m, forecast) + add_changepoints_to_plot(m)

ggplot(df_gas, aes(x=Date)) +
  geom_line(aes(y=Price), color = "#00AFBB") +
  labs(y="Price", x="Year")


# Fan Chart
y0 <- 2004
df_gas0 <- subset(df_gas, Date == y0)
k <- nrow(df_gas0)

p <- seq(0.05, 0.95, 0.05)
p <- c(0.01, p, 0.99)

cpival <- matrix(NA, nrow = length(p), ncol = k)
for (i in 1:k)
  cpival[, i] <- qsplitnorm(p, mode = df_gas0$Price[i])

# past data
plot(cpi, type = "l", col = "tomato", lwd = 2,
     xlim = c(y0 - 5, y0 + 3), ylim = c(-2, 7),
     xaxt = "n", yaxt = "n", ylab = "")

rect(y0 - 0.25, par("usr")[3] - 1, y0 + 3, par("usr")[4] + 1,
     border = "gray90", col = "gray90")
fan(data = cpival, data.type = "values", probs = p,
    start = y0, frequency = 4, anchor = cpi[time(cpi) == y0 - 0.25],
    fan.col = colorRampPalette(c("tomato", "gray90")), ln = NULL, rlab = NULL)
axis(2, at = -2:7, las = 2, tcl = 0.5, labels = FALSE)
axis(4, at = -2:7, las = 2, tcl = 0.5)
axis(1, at = 2008:2016, tcl = 0.5)
axis(1, at = seq(2008, 2016, 0.25), labels = FALSE, tcl = 0.2)
abline(h = 2) # boe cpi target
abline(v = y0 + 1.75, lty = 2) # 2 year line



cpisim <- matrix(NA, nrow = 10000, ncol = k)
for (i in 1:k)
  cpisim[, i] <- rsplitnorm(n = 10000, mode = boe0$mode[i])


# Dygraphs
dygraph(natural_gas_prices)
natural_gas_prices <- cbind(df_gas$Price)

df_gas_timeSeries <- xts(x = df_gas$Price,
                            order.by = df_gas$Date)
interact_time2 <- dygraph(df_gas_timeSeries, main = "US Natural Gas Prices 1997-2020", 
                          ylab = "Natural Gas Price") %>% 
        dyRangeSelector()
interact_time2










































































































