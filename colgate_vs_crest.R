# Importamos las librerias
library(tsibble)
library(feasts)
library(tsoutliers)
library(TSA)
library(xts)
library(tidyverse)
library(forecast)
library(readxl)
library(ggplot2)
# -------------------------------------------------------------------------
# Importamos los datos y preparamos los datos para aplicar los modelos:
data <- read_excel('data.xlsx')

ts_colgate <- ts(data$Colgate, start=c(1958,1), frequency=52.18)
colgate_ts_training <- ts(data$Colgate, start=c(1958,1), end=c(1962,52), frequency=52)
colgate_ts_test <- ts(tail(data$Colgate, 16), start=c(1963,1), end=c(1963,16), frequency=52)

ts_crest <- ts(data$Crest, start=c(1958,1), frequency=52.18)
crest_ts_training <- ts(data$Crest, start=c(1958,1), end=c(1962,52), frequency=52)
crest_ts_test <- ts(tail(data$Crest,16), start=c(1963,1), end=c(1963,16), frequency=52)

# Transformamos los datos en formato tsibble:
crest_ts_tsibble <- as_tsibble(ts_crest)
colgate_ts_tsibble <- as_tsibble(ts_colgate)

# Unimos ambas cotizadas y limpiamos los datos:
data_ts <- crest_ts_tsibble %>% 
  mutate(value_2 = colgate_ts_tsibble$value)
data_ts_tsibble <- as_tsibble(data_ts) %>% 
  mutate(crest = value) %>% 
  mutate(colgate = value_2) %>% 
  select(-c(value, value_2))

# -------------------------------------------------------------------------
# Visualizamos los datos
# plot(colgate_ts_training, type='l', col='blue', ylim=c(0,0.7), main = 'Colgate vs Crest, 1958 to 1963, weekly data')
# lines(crest_ts_training, type='l', col='red')
# legend('topright', legend=c('Colgate (Blue)', 'Crest (Red)'), col=c(1, 2))
# 
# plot(colgate_ts_test, type='l', col='blue', ylim=c(0,0.7), main ='16 weeks on 1963')
# lines(crest_ts_test, typr='l', col='red')
# legend('topright', legend=c('Colgate (Blue)', 'Crest (Red)'), col=c(1, 2))

p <- ggplot(data_ts_tsibble) +
  geom_line(aes(y=crest,
                x=index), colour='blue') +
  geom_line(aes(y=colgate,
                x=index), colour='red')
p + theme(legend.position="bottom")


# -------------------------------------------------------------------------
# ARIMAX V(B) de 15 retardos
# Colgate
arimax_colgate <- arimax(colgate_ts_training,
                         order=c(1,0,0),
                         include.mean=TRUE,
                         xtransf=crest_ts_training,
                         transfer=list(c(0,15)),
                         method="ML")
summary(arimax_colgate)
forecast::tsdisplay(arimax_colgate$residuals)

# Crest
#  arimax_crest <- arimax(crest_ts_training,
#                          order=c(1,0,0),
#                          include.mean=TRUE,
#                          xtransf=colgate_ts_training,
#                          transfer=list(c(0,15)),
#                          method="ML")
# summary(arimax_crest)
# forecast::tsdisplay(arimax_crest$residuals)

# AUTO-ARIMA
# Colgate
autoarima_colgate <- auto.arima(colgate_ts_training,lambda=0)
summary(autoarima_colgate)
ggtsdisplay(autoarima_colgate$residuals)
Box.test(autoarima_colgate$residuals,lag=12, fitdf=3, type="Lj")
colgate_forecast=forecast(autoarima_colgate)
autoplot(colgate_forecast)+ggtitle("ARIMA: Predicción Colgate")

# Crest
autoarima_crest <- auto.arima(crest_ts_training,lambda=0)
summary(autoarima_crest)
ggtsdisplay(autoarima_crest$residuals)
Box.test(autoarima_crest$residuals,lag=12, fitdf=3, type="Lj")
crest_forecast=forecast(autoarima_crest)
autoplot(crest_forecast)+ggtitle("ARIMA: Predicción Crest")

# -------------------------------------------------------------------------
# Outliers
# Colgate
outliers_colgate <- tso(y = as.ts(colgate_ts_training), 
                        types = c("AO", "LS", "TC"),
                        discard.method = "bottom-up", 
                        tsmethod = "auto.arima", 
                        args.tsmethod = list(allowdrift = FALSE, ic = "bic"))
outliers_colgate

# Crest
outliers_crest <- tso(y = as.ts(crest_ts_training), 
                        types = c("AO", "LS", "TC"),
                        discard.method = "bottom-up", 
                        tsmethod = "auto.arima", 
                        args.tsmethod = list(allowdrift = FALSE, ic = "bic"))
outliers_crest

# -------------------------------------------------------------------------
# Modelo de intervención:
# Colgate 
dummies=data.frame(
  LS1960_32=1*(seq(crest_ts_training)>=136),
  AO1959_50=1*(seq(crest_ts_training)==102))

mod_colgate_int = arimax(colgate_ts_training,
                         order=c(3,0,0),
                         seasonal=list(order=c(1,0,0),
                                       period=52),
                         xreg=dummies,
                         method='ML')
mod_colgate_int

# Crest
dummies=data.frame(
  LS1960_32=1*(seq(crest_ts_training)>=136),
  AO1961_11=1*(seq(crest_ts_training)==167),
  TC1961_40=1*(seq(crest_ts_training)<=196))

mod_crest_int = arimax(crest_ts_training,
                         order=c(0,1,1),
                         seasonal=list(order=c(0,0,0),
                                       period=52),
                         xreg=dummies,
                         method='ML')
mod_crest_int

# -------------------------------------------------------------------------
# Plot
# Colgate
plot(log(ts_colgate),ylab='Log(ts_colgate)')
points(fitted(mod_colgate_int))

# Crest
plot(log(ts_crest),ylab='Log(ts_crest)')
points(fitted(mod_crest_int))

# -------------------------------------------------------------------------
# Plot Effect
# Colgate
mod_colgate_int$coef
colgate_plot_effect=1*(seq(ts_colgate)==136)
plot(ts(colgate_plot_effect*(-0.0949)+
        stats::filter(colgate_plot_effect,filter=.8139,method='recursive',side=1)*(-0.2715),
        frequency=52,start=1958),type='h',ylab='ADA Announceemnt Effect')
abline(h=0)

# Crest
mod_crest_int$coef
crest_plot_effect=1*(seq(ts_crest)==136)
plot(ts(crest_plot_effect*(-0.0949)+
        stats::filter(crest_plot_effect,filter=.8139,method='recursive',side=1)*(-0.2715),
        frequency=52,start=1958),type='h',ylab='ADA Announceemnt Effect')
abline(h=0)

# Función de transferencia
mod_colgate_int$coef
plot(mod_colgate_int$coef[0:10],type="h")
