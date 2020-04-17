library(Quandl)
library(tidyverse)
library(tidyquant)
library(timetk)
library(forecast)
library(highcharter)
library(lubridate)

# You might want to supply an API key. It's free to sign up.
Quandl.api_key("")

get()
setwd()

####  wtc is used for actual calculations as of 4/15/20 
####  Unsure of efficacy 
wtc <- read.csv("path")

# Make sure there are no duplicates
deduped <- unique(wtc)

# Data frame not used in final calculation
# Start with daily data. Note that "type = raw" will download a data frame.
oil_daily <- Quandl("FRED/DCOILWTICO", 
                    type = "raw", 
                    collapse = "daily",  
                    start_date = "2008-01-01", 
                    end_date = "2020-04-14")

#### This function does not work on oil_daily
# index(oil_daily) <- seq(mdy('01/01/2008'), mdy(last(index(oil_daily))), by = 'days')

# highchart(type = "stock") %>% 
#   hc_add_series(oil_daily, color = "cornflowerblue") %>% 
#   hc_yAxis(title = list(text = "daily price"),
#            labels = list(format = "${value}"),
#            opposite = FALSE) %>% 
#   hc_add_theme(hc_theme_flat())

#### auto.arima() claims wtc is not a univariate series
#### The analysis on $Value works but is this no longer statistically sound?
auto.arima(wtc$Value) %>%
  forecast(h = 28)
#      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 8636       26.04350 24.55171 27.53529 23.76201 28.32500
# 8637       26.22347 24.16086 28.28609 23.06897 29.37797
# 8638       26.09457 23.60086 28.58827 22.28077 29.90836
# 8639       25.90546 23.03914 28.77177 21.52181 30.28911
# 8640       25.99182 22.77890 29.20474 21.07808 30.90555
# 8641       25.99182 22.49056 29.49307 20.63711 31.34652
# 8642       25.99182 22.22423 29.75941 20.22979 31.75385
# 8643       25.99182 21.97552 30.00812 19.84942 32.13422
# 8644       25.99182 21.74134 30.24230 19.49127 32.49237
# 8645       25.99182 21.51940 30.46424 19.15184 32.83179
# 8646       25.99182 21.30796 30.67567 18.82848 33.15515
# 8647       25.99182 21.10567 30.87796 18.51910 33.46453
# 8648       25.99182 20.91143 31.07221 18.22203 33.76160
# 8649       25.99182 20.72434 31.25929 17.93591 34.04773
# 8650       25.99182 20.54368 31.43996 17.65960 34.32403
# 8651       25.99182 20.36881 31.61482 17.39217 34.59146
# 8652       25.99182 20.19923 31.78441 17.13281 34.85082
# 8653       25.99182 20.03446 31.94917 16.88083 35.10280
# 8654       25.99182 19.87414 32.10950 16.63563 35.34800
# 8655       25.99182 19.71791 32.26573 16.39670 35.58694
# 8656       25.99182 19.56547 32.41816 16.16357 35.82006
# 8657       25.99182 19.41657 32.56706 15.93585 36.04779
# 8658       25.99182 19.27097 32.71266 15.71317 36.27046
# 8659       25.99182 19.12846 32.85518 15.49521 36.48842
# 8660       25.99182 18.98884 32.99479 15.28169 36.70194
# 8661       25.99182 18.85196 33.13168 15.07235 36.91129
# 8662       25.99182 18.71765 33.26599 14.86694 37.11670

# 8663       25.99182 18.58577 33.39786 14.66525 37.31838   ###########################

# I believe this will predict the price on May 4th, 2020 as $25.99182
auto.arima(wtc$Value) %>%
  forecast(h = 28) %>%
  hchart() %>%
  hc_title(text = "Oil Historical and Forecast") %>%
  hc_yAxis(title = list(text = "daily price"), 
  labels = list(format = "${Value}"),
  opposite = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = TRUE)

###################################################################################


# Now weekly and let's use xts as the type.
oil_weekly <- Quandl("FRED/DCOILWTICO", 
                     type = "xts", 
                     collapse = "weekly",  
                    start_date = "2008-01-01", 
                    end_date = "2020-04-10")

# And monthly using xts as the type.
oil_monthly <- Quandl("FRED/DCOILWTICO", 
                      type = "xts", 
                      collapse = "monthly",  
                    start_date = "2008-01-01", 
                    end_date = "2020-04-10")


index(oil_monthly) <- seq(mdy('01/01/2008'), mdy(last(index(oil_monthly))), by = 'months')

index(oil_daily) <- seq(mdy('01/01/2008'), mdy(last(index(oil_daily))), by = 'days')


head(index(oil_monthly))

highchart(type = "stock") %>% 
  hc_add_series(oil_monthly, color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "monthly price"),
           labels = list(format = "${value}"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat())


head(index(oil_daily))

highchart(type = "stock") %>% 
  hc_add_series(oil_daily, color = "cornflowerblue") %>% 
  hc_yAxis(title = list(text = "daily price"),
           labels = list(format = "${value}"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat())

auto.arima(oil_monthly)
## Series: oil_monthly 
## ARIMA(1,1,0) 
## 
## Coefficients:
##          ar1
##       0.3218
## s.e.  0.0871
## 
## sigma^2 estimated as 47.84:  log likelihood=-398.55
## AIC=801.1   AICc=801.2   BIC=806.65

# Series: oil_monthly 
# ARIMA(1,1,0) 

# Coefficients:
#          ar1
#       0.2757
# s.e.  0.0796

# sigma^2 estimated as 49.24:  log likelihood=-494.53
# AIC=993.06   AICc=993.15   BIC=999.04



auto.arima(oil_monthly) %>% 
  forecast(h = 6)
##     Point Forecast    Lo 80    Hi 80    Lo 95     Hi 95
## 121       61.44478 52.58047 70.30909 47.88799  75.00157
## 122       61.76170 47.06935 76.45406 39.29169  84.23172
## 123       61.86370 42.48557 81.24182 32.22741  91.49998
## 124       61.89652 38.60035 85.19270 26.26809  97.52495
## 125       61.90709 35.21664 88.59754 21.08757 102.72661
## 126       61.91049 32.19775 91.62322 16.46878 107.35219

#     Point Forecast      Lo 80    Hi 80       Lo 95    Hi 95
# 149       27.78132 18.7883854 36.77425  14.0278154 41.53482
# 150       28.21448 13.6377947 42.79117   5.9213636 50.50760
# 151       28.33389  9.3540634 47.31372  -0.6932499 57.36103
# 152       28.36681  5.7260614 51.00755  -6.2592257 62.99284
# 153       28.37588  2.5639120 54.18785 -11.1001196 67.85188
# 154       28.37838 -0.2619262 57.01869 -15.4231905 72.17996


auto.arima(oil_monthly) %>% 
  forecast(h = 6) %>% 
  hchart() %>% 
  hc_title(text = "Oil historical and forecast") %>% 
  hc_yAxis(title = list(text = "monthly price"),
           labels = list(format = "${value}"),
           opposite = FALSE) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = TRUE)

