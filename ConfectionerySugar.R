###############################################################################
#CONFECTIONERY ITS REGRESSION
#DAVID PELL
#15/01/19
###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/dph/sdil/KWPITS/ITSData.R")
}

###############################################################################
#SUBSET WEEKLYVOLUME TO CATEGORY OF INTEREST
###############################################################################

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="Confectionery"]

###############################################################################
#ADD CONTROL AND INTERACTION VARIABLES
###############################################################################

source("/dph/sdil/KWPITS/ControlDataSetup.R")

###############################################################################
#CONVERT HIGHER TIER SUGAR TO KG
###############################################################################

SDIL$sugar <- ifelse(SDIL$SDIL2=="Confectionery",(SDIL$sugar/1000),SDIL$sugar)

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
# summary(lm(sugar ~ Time + Indicator + IndicatorTime + Level + Trend + 
#              I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
#              AverageMonthlyTemperature + December + Easter + 
#              January, data=SDIL))
# 
# ###############################################################################
# #REDUCED OLS MODEL
# #DROP QUADRATIC TERM
# ###############################################################################
# 
# model_ols <- lm(sugar ~ Time + Indicator + IndicatorTime + Level + Trend +
#                   IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December +
#                   January + Easter, data=SDIL)
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
# abline(h=0.04, lty=3)
# abline(h=-0.04, lty=3)
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
# ARIMA <- gls(sugar ~ Time + Indicator + IndicatorTime + Level + Trend +
#                IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December +
#                January + Easter, data=SDIL,method="ML")
# 
# ###############################################################################
# #CORRELATION STRUCTURE CHECKS - P
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=2,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=3,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=5,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=6,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=14,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=15,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=21,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=22,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=23,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=24,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=25,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=26,form=~Time|Indicator)))
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=1,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=2,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=3,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=9,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=10,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=21,form=~Time|Indicator))) #NON-COVERGENCE ERROR
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - P AND Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1, q=1,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=2, q=2,form=~Time|Indicator))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=3, q=3,form=~Time|Indicator))) 

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

ConfectionerySugar <- gls(sugar ~ Time + Indicator + IndicatorTime + Level + Trend + IndicatorLevel + 
                       IndicatorTrend + AverageMonthlyTemperature + December + January + Easter, 
                     data=SDIL,correlation=corARMA(p=3, q=3,form=~Time|Indicator),method="ML")
