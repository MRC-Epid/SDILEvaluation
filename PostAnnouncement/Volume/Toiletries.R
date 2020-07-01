###############################################################################
#TOILETRIES ITS REGRESSION
#DAVID PELL
#25/04/2019
###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/dph/sdil/KWPITS/ITSData.R")
}

###############################################################################
#SUBSET WEEKLYVOLUME TO CATEGORY OF INTEREST
###############################################################################

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="Toiletries"]

##############################################################################
#EMPIRICAL PLOT
###############################################################################

# ggplot(SDIL,aes(x=Weeks,y=volume)) +
#   geom_line() + 
#   geom_point() + 
#   geom_vline(xintercept=as.Date("2016-03-16")) 
# 
# ###############################################################################
# #OLS MODEL
# ###############################################################################
# 
# summary(lm(volume ~ Time + Level + Trend, data=SDIL))
# 
# ###############################################################################
# #REDUCED OLS MODEL
# #DROP EASTER + QUADRATIC TERM
# ###############################################################################
# 
# model_ols <- lm(volume ~ Time + Level + Trend, data=SDIL)
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
# abline(h=0.05, lty=3)
# abline(h=-0.05, lty=3)
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
# ARIMA <- gls(volume ~ Time + Level + Trend, data=SDIL,correlation=corARMA(p=4,form=~Time),method="ML")
# 
# ###############################################################################
# #CORRELATION STRUCTURE CHECKS - P
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=1,form=~Time))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=8,form=~Time))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=12,form=~Time))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=20,form=~Time))) 
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=4,form=~Time))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=8,form=~Time))) 
# anova(ARIMA,update(ARIMA,correlation=corARMA(q=12,form=~Time))) 
# 
# ###############################################################################
# #CONTINUE CORRELATION STRUCTURE CHECKS - P AND Q
# ###############################################################################
# 
# anova(ARIMA,update(ARIMA,correlation=corARMA(p=4, q=4,form=~Time)))

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

Toiletries <- gls(volume ~ Time + Level + Trend, data=SDIL,correlation=corARMA(p=4,q=4,form=~Time),method="ML")
