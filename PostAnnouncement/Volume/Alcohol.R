###############################################################################
#ALCOHOL ITS REGRESSION
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

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="Alcohol"]

###############################################################################
#ADD CONTROL AND INTERACTION VARIABLES
###############################################################################

source("/dph/sdil/KWPITS/ControlDataSetup.R")

##############################################################################
#EMPIRICAL PLOT
###############################################################################

# ggplot(SDIL,aes(x=Weeks,y=volume, colour=SDIL2)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 
# 
# ##############################################################################
# #EMPIRICAL PLOT - ALCOHOL ONLY
# ###############################################################################
# 
# ggplot(SDIL[SDIL$SDIL2=="Alcohol"],aes(x=Weeks,y=volume, colour=SDIL2)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) +
#   scale_color_manual(values=c("#00BFC4"))
# 
# ##############################################################################
# #EMPIRICAL PLOT - CONTROL ONLY
# ###############################################################################
# 
# ggplot(SDIL[SDIL$SDIL2=="Toiletries"],aes(x=Weeks,y=volume, colour=SDIL2)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) +
#   scale_color_manual(values=c("#00BFC4"))
# 
# ###############################################################################
# #OLS MODEL
# ###############################################################################
# 
# summary(lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
#                   I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
#                   AverageMonthlyTemperature + December + Easter + 
#                   January, data=SDIL))
# 
# ###############################################################################
# #REDUCED OLS MODEL
# #DROP QUADRATIC TERM + EASTER
# ###############################################################################
# 
# model_ols <- lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
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
# abline(h=0.3, lty=3)
# abline(h=-0.3, lty=3)
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
#                IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December + January,
#              data=SDIL,method="ML")
# 
# ###############################################################################
# #CORRELATION STRUCTURE CHECKS - P
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=2,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=15,form=~Time|Indicator))) 
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=2,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=4,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=15,form=~Time|Indicator))) 
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - P AND Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1, q=1,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=2, q=2,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4, q=4,form=~Time|Indicator))) #NON-CONVERGENCE ERROR

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

Alcohol <- gls(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
                 IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December + January,
               data=SDIL,correlation=corARMA(p=1, q=1,form=~Time|Indicator),method="ML")
