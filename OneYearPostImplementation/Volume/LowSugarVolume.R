###############################################################################
#LOW SUGAR ITS REGRESSION
#DAVID PELL
#05/11/19
###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/dph/sdil/KWP221/Cleaning/ITSData19.R")
}

###############################################################################
#SUBSET WEEKLYVOLUME TO CATEGORY OF INTEREST
###############################################################################

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2 %in% c("LowSugar","Toiletries")]

###############################################################################
#ORDER DATA
###############################################################################

SDIL <- SDIL[order(SDIL$SDIL2 %in% "Toiletries", decreasing=FALSE)]

##############################################################################
#EMPIRICAL PLOT
###############################################################################
# 
# ggplot(SDIL, aes(x=Weeks,y=volume, colour=SDIL2)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) + 
#   geom_vline(xintercept=as.Date("2018-04-06"))
# 
###############################################################################
#CREATE FORMULA
###############################################################################
# 
# Model <- as.formula(volume ~ Time + SDILAnnouncement + SDILImplementation + 
#                       AnnouncementTrend + ImplementationTrend + 
#                       Indicator + IndicatorTime + 
#                       AnnouncementIndicatorLevel + AnnouncementIndicatorTrend + 
#                       ImplementationIndicatorLevel + ImplementationIndicatorTrend +              
#                       AverageMonthlyTemperature + December + January + Easter)
# 
###############################################################################
#MODEL
###############################################################################
# 
# model_ols <- summary(lm(Model,data=SDIL))
# 
###############################################################################
#UPDATE FORMULA
#DROP EASTER
###############################################################################
# 
Model <- as.formula(volume ~ Time + SDILAnnouncement + SDILImplementation + 
                      AnnouncementTrend + ImplementationTrend + 
                      Indicator + IndicatorTime + 
                      AnnouncementIndicatorLevel + AnnouncementIndicatorTrend + 
                      ImplementationIndicatorLevel + ImplementationIndicatorTrend +              
                      AverageMonthlyTemperature + December + January)

###############################################################################
#REDUCED OLS MODEL
###############################################################################
# 
# model_ols <- lm(Model, data=SDIL)
# 
# ###############################################################################
# #PERFORM TEST FOR AUTOCORRELATION
# #A rule of thumb is that test statistic values in the range of 1.5 to 2.5 are relatively normal. 
# #Values outside of this range could be cause for concern. 
# #Field(2009) suggests that values under 1 or more than 3 are a definite cause for concern.
# #https://www.statisticshowto.datasciencecentral.com/durbin-watson-test-coefficient/
# ###############################################################################
# 
# dwt(model_ols, max.lag=24, alternative="two.sided")
# 
# ###############################################################################
# #PLOT RESIDUALS
# ###############################################################################
# 
# plot(SDIL$Weeks[1:264],residuals(model_ols)[1:264],type="o", pch=16, xlab="Time",ylab="OLS Residuals",col="red")
# abline(h=0, lty=3)
# abline(h=0.03, lty=3)
# abline(h=-0.03, lty=3)
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
# ARIMA <- gls(Model,data=SDIL, method="ML")
# 
# ###############################################################################
# #CORRELATION STRUCTURE CHECKS - P
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=8,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=12,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=15,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=19,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=20,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=24,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=27,form=~Time|Indicator)))
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=4,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=8,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=15,form=~Time|Indicator)))
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=20,form=~Time|Indicator))) #NON CONVERGENCE ERROR
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=24,form=~Time|Indicator))) #NON CONVERGENCE ERROR
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=27,form=~Time|Indicator))) #NON CONVERGENCE ERROR
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - P AND Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4, q=4,form=~Time|Indicator))) #NON CONVERGENCE ERROR
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=8, q=8,form=~Time|Indicator))) #NON CONVERGENCE ERROR

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

LowSugar <- gls(Model, data=SDIL, correlation=corARMA(p=4, form=~Time|Indicator),method="ML")

###############################################################################
#TIDY UP
###############################################################################

suppressWarnings(rm(SDIL,Model,model_ols))
