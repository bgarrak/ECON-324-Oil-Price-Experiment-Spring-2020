library(Quandl)
library(tidyverse)
library(tidyquant)
library(timetk)
library(forecast)
library(highcharter)
library(lubridate)

# You might want to supply an API key. It's free to sign up.
Quandl.api_key("")


# Start with daily data. Note that "type = raw" will download a data frame.
oil_daily <- Quandl("FRED/DCOILWTICO", 
                    type = "raw", 
                    collapse = "daily",  
                    start_date = "2008-01-01", 
                    end_date = "2020-04-14")

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

