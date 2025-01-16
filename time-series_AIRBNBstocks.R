##airbnb 
library(fGarch)
library(tidyquant) 
library(fpp3)
library(tseries)
library(data.table)
library(plotly)
library(dplyr)
library(tsibble)
library(ggplot2)
library(timetk)

setwd("/Users/shok/Desktop/time-series/final_project/datasets")
da=read.csv("ABNB.csv",header=T)
head(da)

data <- data.frame(Date = da$Date, AdjClose = as.numeric(as.character(da$Adj.Close)))
data <- data %>%
  mutate(Return = difference(log(AdjClose)) * 100)
View(data)
data<- na.omit(data)
data$Date <- as.Date(data$Date)

##PLOTTING----
data %>%
  ggplot(aes(x = Date, y = AdjClose)) +
  geom_line(color="blue") +
  labs(title = " Adj Close Price ")
data$Date <- as.Date(data$Date)
data %>%
  ggplot(aes(x = Date, y = Return)) +
  geom_line(color="blue") +
  labs(title = " Return ")

##DESCRIPTIVE ----
library(fBasics)
basicStats(data$Return) 

##SIMPLE ANALYSIS ----
t.test(data$Return)
normalTest(data$Return, method=c('jb'))
##check for skewness and kurt
skew = skewness(data$Return)
T=length(data$Return)
s3=skew/sqrt(6/T)
p3=2*(1-pnorm(abs(s3))) 
p3
#alternative
kurt=kurtosis(data$Return)
kurt 
k4=kurt/sqrt(24/T)
p4=2*(1-pnorm(k4)) 
p4

##ADF TEST----
model = ar(data$Return, method="mle")
model$order
library(fUnitRoots)
adfTest(data$Return, type="c", lags=3)

##ACF & PACF ----
Box.test(data$Return, lag=12, type='Ljung')
par(mfcol=c(2,1))
acf(data$Return, lag=24)
pacf(data$Return, lag=24)


#ARMA model----
arma_3=arima(data$Return,order=c(2,0,2)) 
arma_3
Box.test(arma_3$residuals,lag=20,type='Ljung') 
tsdiag(arma_3,gof=20)

#Check the model ARMA (2,2)
plot(arma_3$residuals)
abline(h=mean(arma_3$residuals), col="red")
acf(arma_3$residuals)
qqnorm(arma_3$residuals)
qqline(arma_3$residuals)



#(G)ARCH model estimation----
par(mfcol=c(2,1))
acf(data$Return)
Box.test(data$Return, lag=12, type='Ljung')
###absolute returns ----
acf(abs(data$Return), lag=24) ##take absolute values 
Box.test(abs(data$Return), lag=12, type='Ljung')
###ARCH test ----
mean(data$Return)
t.test(data$Return)
y = data$Return
Box.test(y, lag=12, type = "Ljung") # test1
Box.test(y^2, lag=12, type = "Ljung")
source("archTest.R")
archTest(y, 12) #test 2
###ARCH estimation ----
par(mfcol=c(2,1))
acf(y^2)
pacf(y^2) #use PACF to get lag p for ARCH(p)
###ARCH(2) model ----
arch2=garchFit(~1+garch(2,0), data=data$Return,trace=F)
summary(arch2)
###Run GARCH models----
garch11=garchFit(~1+garch(1,1), data=data$Return,trace=F)
summary(garch11)
plot(garch11)
garch12=garchFit(~1+garch(1,2), data=data$Return,trace=F)
summary(garch12)
garch21=garchFit(~1+garch(2,1), data=data$Return,trace=F)
summary(garch21)
##GARCH models with conditions
garch11_std=garchFit(~1+garch(1,1),data=data$Return,trace=F,cond.dist="std")
summary(garch11_std)
plot(garch11_std)
garch11_sstd=garchFit(~1+garch(1,1),data=data$Return,trace=F,cond.dist="sstd")
summary(garch11_sstd)
plot(garch11_sstd)
par(mfcol=c(3,1))
plot(garch11)
plot(garch11_std)
plot(garch11_sstd)

##Forecasting (80 * 20) ----
data1 <- data[1:591,]
model_forecast=garchFit(~1+garch(1,1), data=data1$Return,trace=F)
library(forecast)
length(data$Return)
data2 <- data[592:739,]
View(data2)
nrow(data2)
predictions=predict(model_forecast,148, plot=TRUE)
CIupper=predictions$meanForecast + 1.96*predictions$meanError
CIlower=predictions$meanForecast - 1.96*predictions$meanError
print(cbind(CIupper,CIlower,data2$Return))

####plotting
plot_data <- data.frame(
  Date = data2$Date,  # Replace with your actual date column name
  Predicted = predictions$meanForecast,
  CIupper = CIupper,
  CIlower = CIlower,
  Actual = data2$Return
)

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Predicted), color = "purple", linetype = "dashed") +
  geom_ribbon(aes(ymin = CIlower, ymax = CIupper), fill = "purple", alpha = 0.3) +
  geom_line(aes(y = Actual), color = "red") +
  labs(title = "Predicted vs Actual Values with 95% Confidence Intervals", y = "Return") +
  theme_minimal()


##Kenneth [EVENT STUDY]----
french=read.csv("french.csv",header=T)
french$Date <- as.Date(french$Date, format = "%Y%m%d")
#View(french)
merged_dataset <- merge(data, french, by = "Date", all = TRUE)
#View(merged_dataset)
final_merged <- na.omit(merged_dataset)
nrow(final_merged)
View(final_merged)
final_merged$abnormal <- final_merged$Return - final_merged$RF

####Airbnb 2021 Summer Release (May 24, 2021)----
position <- which(final_merged$Date == "2021-05-24")
print(position)
subset_data <- final_merged[107:117, ]
View(subset_data)
subset_data$CAR <- cumsum(subset_data$abnormal)
subset_data$x_labels <- seq(-5, 5)
subset_data$x_labels <- as.numeric(subset_data$x_labels)

#plotting
library(ggplot2)
ggplot(subset_data, aes(x = x_labels, y = CAR)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Cumulative Abnormal Returns",
       x = "Time",
       y = "Cumulative Abnormal Return",
       subtitle = "Airbnb 2021 Summer Release") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_continuous(breaks = subset_data$x_labels, labels = subset_data$x_labels)

####Airbnb 2021 Winter Release (November 9, 2021)----
position <- which(final_merged$Date == "2021-11-09")
print(position)
subset_data <- final_merged[225:235, ]
#View(subset_data)
subset_data$CAR <- cumsum(subset_data$abnormal)
subset_data$x_labels <- seq(-5, 5)
subset_data$x_labels <- as.numeric(subset_data$x_labels )
## plot 
ggplot(subset_data, aes(x = x_labels, y = CAR)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Cumulative Abnormal Return",
       x = "Time",
       y = "Cumulative Abnormal Return",
       subtitle = "Airbnb 2021 Winter Release") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_continuous(breaks = subset_data$x_labels, labels = subset_data$x_labels)


####Airbnb 2022 Summer Release (May 11, 2022) ----
position <- which(final_merged$Date == "2022-05-11")
print(position)
subset_data <- final_merged[351:361, ]
#View(subset_data)
subset_data$CAR <- cumsum(subset_data$abnormal)
subset_data$x_labels <- seq(-5, 5)
subset_data$x_labels <- as.numeric(subset_data$x_labels )
##Plot
ggplot(subset_data, aes(x = x_labels, y = CAR)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Cumulative Abnormal Return",
       x = "Time",
       y = "Cumulative Abnormal Return",
       subtitle = "Airbnb 2022 Summer Release") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_continuous(breaks = subset_data$x_labels, labels = subset_data$x_labels)

####Airbnb 2022 Winter Release (Nov 16, 2022)----
position <- which(final_merged$Date == "2022-11-16")
print(position)
subset_data <- final_merged[482:492, ]
#View(subset_data)
subset_data$CAR <- cumsum(subset_data$abnormal)
subset_data$x_labels <- seq(-5, 5)
subset_data$x_labels <- as.numeric(subset_data$x_labels )
##plot
ggplot(subset_data, aes(x = x_labels, y = CAR)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Cumulative Abnormal Return",
       x = "Time",
       y = "Cumulative Abnormal Return",
       subtitle = "Airbnb 2022 Winter Release") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_continuous(breaks = subset_data$x_labels, labels = subset_data$x_labels)


#####20 days window----
####Airbnb 2022 Summer Release (May 11, 2022) ----
position <- which(final_merged$Date == "2022-05-11")
print(position)
lower <- position - 20
upper <- position + 20
subset_data <- final_merged[lower:upper,]
#View(subset_data)
subset_data$CAR <- cumsum(subset_data$abnormal)
subset_data$x_labels <- seq(-20, 20)
##plot
ggplot(subset_data, aes(x = x_labels, y = CAR)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Cumulative Abnormal Return",
       x = "Time",
       y = "Cumulative Abnormal Return",
       subtitle = "Airbnb 2022 Summer Release") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none") +
  scale_x_continuous(breaks = subset_data$x_labels, labels = subset_data$x_labels)

##Booking.com ----
booking=read.csv("BKNG.csv",header=T)
booking$Date <- as.Date(booking$Date)
booking <- booking %>%
  mutate(booking_Return = difference(log(Adj.Close)) * 100)
View(booking)
booking<- na.omit(booking)
#View(booking)
merged_booking <- merge(data, booking, by = "Date", all = TRUE)
#View(merged_booking)
final_merged <- na.omit(merged_booking)
#View(final_merged)

correlation_data <- data.frame(Book_Return =final_merged$booking_Return, AirBnB_Return = final_merged$Return)
correlation_coefficient <- cor(correlation_data$Book_Return, correlation_data$AirBnB_Return)
ggplot(correlation_data, aes(x = Book_Return, y = AirBnB_Return)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  geom_text(x = max(correlation_data$Book_Return),
            y = min(correlation_data$AirBnB_Return), 
            label = paste("Correlation Coefficient:", round(correlation_coefficient, 3)),
            hjust = 1, vjust = 0, col = "black", size = 4) + 
            labs(title = "Correlation Scatter Plot with Regression Line",
                 x = "Booking Return", y = "AirBnb Return")

#cut data
correlation_data_1 <- data.frame(Book_Return = final_merged$booking_Return, AirBnB_Return = final_merged$Return,
                                 Date = final_merged$Date)
#View(correlation_data_1)
correlation_data_1 <- subset(correlation_data_1, Date > "2022-01-01")
correlation_coefficient <- cor(correlation_data_1$Book_Return, correlation_data_1$AirBnB_Return)
ggplot(correlation_data_1, aes(x = Book_Return, y = AirBnB_Return)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") + 
  geom_text(x = max(correlation_data_1$Book_Return),
            y = min(correlation_data_1$AirBnB_Return), 
            label = paste("Correlation Coefficient:", round(correlation_coefficient, 3)),
            hjust = 1, vjust = 0, col = "black", size = 4) + 
  labs(title = "Correlation Scatter Plot with Regression Line",
       x = "Booking Return", y = "AirBnb Return")
###VAR----
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
#check for stationarity 
pp.test(correlation_data$Book_Return)
pp.test(correlation_data$AirBnB_Return)
#bind values 
v1 <- cbind(correlation_data$AirBnB_Return,correlation_data$Book_Return)
colnames(v1) <- cbind("Airbnb", "Booking")
lagselect <- VARselect(v1, lag.max = 15, type = "const")
lagselect$selection
Model1 <- VAR(v1, p=1, type="const", season=NULL, exog=NULL)
summary(Model1)
Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1
Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE) 
Arch1
Norm1 <- normality.test(Model1, multivariate.only = TRUE) 
Norm1
Stability1 <- stability(Model1, type = "OLS-CUSUM") 
plot(Stability1)
GrangerRRP<- causality(Model1, cause = "Airbnb") 
GrangerRRP
GrangerRRP<- causality(Model1, cause = "Booking") 
GrangerRRP
