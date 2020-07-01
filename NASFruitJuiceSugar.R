###############################################################################
#NAS FRUIT JUICE ITS REGRESSION
#DAVID PELL
#17/01/2019
###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/dph/sdil/KWPITS/ITSData.R")
}

###############################################################################
#SUBSET WEEKLYVOLUME TO CATEGORY OF INTEREST
###############################################################################

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="NASFruitJuice"]

###############################################################################
#ADD CONTROL AND INTERACTION VARIABLES
###############################################################################

source("/dph/sdil/KWPITS/ControlDataSetup.R")

###############################################################################
#CONVERT SUGAR TO KG
###############################################################################

SDIL$sugar <- ifelse(SDIL$SDIL2=="NASFruitJuice",(SDIL$sugar/100),SDIL$sugar)

##############################################################################
#EMPIRICAL PLOT
###############################################################################

# ggplot(SDIL,aes(x=Weeks,y=sugar, colour=SDIL2)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 
# 
# ###############################################################################
# #OLS MODEL
# ###############################################################################
# 
# summary(lm(sugar ~ Time + Indicator + IndicatorTime + Level + Trend + I(IndicatorTrend*IndicatorTrend) + 
#              IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December + Easter + January, data=SDIL))
# 
# ###############################################################################
# #REDUCED OLS MODEL
# #DROP EASTER
# ###############################################################################
# 
# model_ols <- lm(sugar ~ Time + Indicator + IndicatorTime + Level + Trend + I(IndicatorTrend*IndicatorTrend) +
#                   IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + 
#                   December + January, data=SDIL)
# 
# ###############################################################################
# #PERFORM TEST FOR AUTOCORRELATION
# ###############################################################################
# 
# dwt(model_ols, max.lag=24, alternative="two.sided")
# 
# ###############################################################################
# #PLOT RESIDUALS
# ###############################################################################
# 
# plot(SDIL$Weeks[1:211],residuals(model_ols)[1:211],type="o", pch=16, xlab="Time",ylab="OLS Residuals",col="red")
# abline(h=0, lty=3)
# abline(h=0.01, lty=3)
# abline(h=-0.01, lty=3)
# 
# ###############################################################################
# #ACF PLOTS
# ###############################################################################
# 
# par(mfrow=c(2,1))
# acf(residuals(model_ols))
# acf(residuals(model_ols), type="partial")
# par(mfrow=c(1,1))
# 
# ###############################################################################
# #ARIMA MODEL
# ###############################################################################
# 
# ARIMA <- gls(sugar ~ Time + Indicator + IndicatorTime + Level + Trend + IndicatorLevel + 
#                IndicatorTrend + AverageMonthlyTemperature + December + January,
#              data=SDIL,method="ML")
# 
# ###############################################################################
# #CORRELATION STRUCTURE CHECKS - P
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=8,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=12,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=16,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=19,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=20,form=~Time|Indicator)))
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=1,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=4,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=12,form=~Time|Indicator)))

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

NASFruitJuiceSugar <- gls(sugar ~ Time + Indicator + IndicatorTime + Level + Trend + IndicatorLevel + 
                       IndicatorTrend + AverageMonthlyTemperature + December + January,
                     data=SDIL,correlation=corARMA(p=4,form=~Time|Indicator),method="ML")
