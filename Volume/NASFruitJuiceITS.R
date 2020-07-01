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

##############################################################################
#EMPIRICAL PLOT
###############################################################################
# 
# ggplot(SDIL,aes(x=Weeks,y=volume, colour=SDIL2)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 
# 
# ###############################################################################
# #OLS MODEL
# ###############################################################################
# 
# summary(lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + I(IndicatorTrend*IndicatorTrend) + 
#              IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December + Easter + January, data=SDIL))
# 
# ###############################################################################
# #REDUCED OLS MODEL
# #DROP EASTER
# ###############################################################################
# 
# model_ols <- lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
#                   I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
#                   AverageMonthlyTemperature + December + January, data=SDIL)
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
# abline(h=0.2, lty=3)
# abline(h=-0.2, lty=3)
# 
# ###############################################################################
# #ACF PLOTS
# ###############################################################################
# 
# par(mfrow=c(2,1))
# acf(residuals(model_ols))
# acf(residuals(model_ols), type="partial") #looks like autoregressive p=4
# par(mfrow=c(1,1))
# 
# ###############################################################################
# #ARIMA MODEL
# ###############################################################################
# 
# ARIMA <- gls(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
#                I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
#                AverageMonthlyTemperature + December,
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
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=13,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=15,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=16,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=17,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=19,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=20,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=21,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=23,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=24,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=25,form=~Time|Indicator)))
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=4,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=7,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=8,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=12,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=13,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=15,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=19,form=~Time|Indicator))) #NON-CONVERGENCE ERROR
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=24,form=~Time|Indicator))) #NON-CONVERGENCE ERROR
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - P AND Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1, q=1,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=2, q=2,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4, q=4,form=~Time|Indicator))) #NON CONVERGENCE ERROR

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

NASFruitJuice <- gls(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
                       I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
                       AverageMonthlyTemperature + December + January,
                     data=SDIL,correlation=corARMA(p=2,q=2,form=~Time|Indicator),method="ML")
