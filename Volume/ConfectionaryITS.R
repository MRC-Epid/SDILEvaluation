###############################################################################
#CONFECTIONARY ITS REGRESSION
#DAVID PELL
#15/01/19
###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/home/dap49/KWPITS/ITSData.R")
}

###############################################################################
#SUBSET WEEKLYVOLUME TO CATEGORY OF INTEREST
###############################################################################

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="Confectionary"]

###############################################################################
#ADD CONTROL AND INTERACTION VARIABLES
###############################################################################

source("/home/dap49/KWPITS/ControlDataSetup.R")

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
# summary(lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
#              I(Trend*Trend) + IndicatorLevel + IndicatorTrend + 
#              AverageMonthlyTemperature + December + Easter + 
#              January, data=SDIL))
# 
# ###############################################################################
# #REDUCED OLS MODEL
# #DROP INTERACTION
# ###############################################################################
# 
# model_ols <- lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend +
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
# plot(SDIL$Weeks[1:212],residuals(model_ols)[1:212],type="o", pch=16, xlab="Time",ylab="OLS Residuals",col="red")
# abline(h=0, lty=3)
# abline(h=0.1, lty=3)
# abline(h=-0.1, lty=3)
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
# ARIMA <- gls(volume ~ Time + Indicator + IndicatorTime + Level + Trend +
#                IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December +
#                January + Easter, data=SDIL,
#              correlation=corARMA(p=1,form=~Time|Indicator),method="ML")
# 
# ###############################################################################
# #CORRELATION STRUCTURE CHECKS - P
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=2,form=~Time|Indicator))) #q1 better fit than p2
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=3,form=~Time|Indicator))) #q1 better fit than p3
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4,form=~Time|Indicator))) #q1 better fit than p4
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=5,form=~Time|Indicator))) #q1 better fit than p5
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=6,form=~Time|Indicator))) #q1 better fit than p6
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=7,form=~Time|Indicator))) #q1 better fit than p7
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=8,form=~Time|Indicator))) #q1 better fit than p8
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=9,form=~Time|Indicator))) #q1 better fit than p9
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=10,form=~Time|Indicator))) #q1 better fit than p10
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=1,form=~Time|Indicator))) #q1 better fit than q1
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=2,form=~Time|Indicator))) #q1 better fit than q2
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=3,form=~Time|Indicator))) #q1 better fit than q3
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=4,form=~Time|Indicator))) #q1 better fit than q4
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=5,form=~Time|Indicator))) #q1 better fit than q5
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=6,form=~Time|Indicator))) #q1 better fit than q6
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=7,form=~Time|Indicator))) #q1 better fit than q7
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=8,form=~Time|Indicator))) #q1 better fit than q8
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=9,form=~Time|Indicator))) #q1 better fit than q9
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=10,form=~Time|Indicator)))#q1 better fit than q10
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=11,form=~Time|Indicator)))#q1 better fit than q10
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=12,form=~Time|Indicator)))#q12 gives non-convergence error
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - P AND Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=11, q=11,form=~Time|Indicator))) #non convergence error

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

Confectionary <- gls(volume ~ Time + Indicator + IndicatorTime + Level + Trend +
                   IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December +
                   January + Easter, data=SDIL,
               correlation=corARMA(q=1,form=~Time|Indicator),method="ML")
