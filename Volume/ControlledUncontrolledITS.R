###############################################################################
#LOWER TIER ITS REGRESSION
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

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="LowerTier"]

###############################################################################
#UNCONTROLLED MODEL
###############################################################################

Uncontrolled <- gls(volume ~ Time + Level + Trend + I(Trend*Trend) + 
                   AverageMonthlyTemperature + December + January, 
                 data=SDIL, method="ML")

###############################################################################
#ADD CONTROL AND INTERACTION VARIABLES
###############################################################################

source("/dph/sdil/KWPITS/ControlDataSetup.R")

###############################################################################
#CONTROLLED MODEL
###############################################################################

Controlled <- gls(volume ~ Time +  Level + Trend + I(IndicatorTrend*IndicatorTrend) + 
                   + AverageMonthlyTemperature + December + January + 
                   Indicator + IndicatorTime + IndicatorLevel + IndicatorTrend , 
                 data=SDIL, method="ML")

###############################################################################
#PLOTTING
###############################################################################

plot(SDIL$Weeks,SDIL$volume, ylim=c(0,round(max(SDIL$volume)*1.02,4)), 
     col="white", ylab="Mean Weekly Purchased Household Volume (ml)", xlab="", yaxs="i",
     xaxt="n", yaxt="n")

###############################################################################
#ADD X AXIS LABELS
###############################################################################

axis.Date(side = 1, at = seq(from = min(SDIL$Weeks), to = max(SDIL$Weeks), 
                             length.out=9), format = "%b-%y", cex.axis=1.2)

###############################################################################
#ADD Y AXIS VALUES
###############################################################################

axis(side = 2, at = seq(from = 0, to = 6, length.out=41),
     labels=seq(from = 0, to = 6,length.out=41)*1000)

###############################################################################
#ADD DATA POINTS
###############################################################################

points(x=SDIL$Weeks[1:211], y=SDIL$volume[1:211], col="gray65",lwd=2, pch=3, cex=0.5)
points(x=SDIL$Weeks[1:211], y=SDIL$volume[212:422], col="gray85",lwd=2, pch=1, cex=0.5) 

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=SDIL$Weeks[1:106], y=coefficients(Controlled)[["(Intercept)"]] + coefficients(Controlled)[["Time"]] * 1:106, col="gray40",lwd=2)

###############################################################################
#POST ANNOUNCEMENT REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=SDIL$Weeks[107:211],
      y=coefficients(Controlled)[["(Intercept)"]] + coefficients(Controlled)[["Time"]] * 107:211 + coefficients(Controlled)[["Level"]]  + 
        coefficients(Controlled)[["Trend"]] * 1:105, col="gray40",lwd=2)

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[1:106],
      y=coefficients(Controlled)[["(Intercept)"]] + coefficients(Controlled)[["Indicator"]] + 
        (coefficients(Controlled)[["Time"]] + coefficients(Controlled)[["IndicatorTime"]]) * 1:106 + 
        mean(SDIL$AverageMonthlyTemperature[1:211])* coefficients(Controlled)[["AverageMonthlyTemperature"]] +  
        mean(SDIL$December[1:211])* coefficients(Controlled)[["December"]] +  
        mean(SDIL$January[1:211])* coefficients(Controlled)[["January"]],
      col="goldenrod",lwd=2)

###############################################################################
#COUNTERFACTUAL - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[107:211],
      y=coefficients(Controlled)[["(Intercept)"]] + 
        coefficients(Controlled)[["Indicator"]] + 
        (coefficients(Controlled)[["Time"]] + coefficients(Controlled)[["IndicatorTime"]]) * 107:211 + 
        coefficients(Controlled)[["Level"]]  + 
        coefficients(Controlled)[["Trend"]] * 1:105 + 
        mean(SDIL$AverageMonthlyTemperature[1:211])* coefficients(Controlled)[["AverageMonthlyTemperature"]] +  
        mean(SDIL$December[1:211])* coefficients(Controlled)[["December"]] +  
        mean(SDIL$January[1:211])* coefficients(Controlled)[["January"]],
      col="goldenrod",lwd=2, lty="dashed")

###############################################################################
#POST ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[107:211],
      y=coefficients(Controlled)[["(Intercept)"]] + 
        coefficients(Controlled)[["Indicator"]] + 
        (coefficients(Controlled)[["Time"]] + coefficients(Controlled)[["IndicatorTime"]]) * 107:211 + 
        coefficients(Controlled)[["Level"]]  + 
        coefficients(Controlled)[["IndicatorLevel"]] + 
        (coefficients(Controlled)[["Trend"]] + coefficients(Controlled)[["IndicatorTrend"]]) * 1:105 +  
        coefficients(Controlled)[["I(IndicatorTrend * IndicatorTrend)"]] * (SDIL$IndicatorTrend[107:211]^2) +
        mean(SDIL$AverageMonthlyTemperature[1:211])* coefficients(Controlled)[["AverageMonthlyTemperature"]] +  
        mean(SDIL$December[1:211])* coefficients(Controlled)[["December"]] +  
        mean(SDIL$January[1:211])* coefficients(Controlled)[["January"]], 
      col="goldenrod",lwd=2)

#_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~#
#_~_~_~_~_~_~_~_~_~UNCONTROLLED_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~#
#_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~_~#

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[1:106],
      y=coefficients(Uncontrolled)[["(Intercept)"]] + 
        coefficients(Uncontrolled)[["Time"]] * 1:106 + 
        mean(SDIL$AverageMonthlyTemperature[1:211])* coefficients(Uncontrolled)[["AverageMonthlyTemperature"]] +  
        mean(SDIL$December[1:211])* coefficients(Uncontrolled)[["December"]] +  
        mean(SDIL$January[1:211])* coefficients(Uncontrolled)[["January"]],
      col="red",lwd=2)

###############################################################################
#COUNTERFACTUAL - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[107:211],
      y=coefficients(Uncontrolled)[["(Intercept)"]] + 
        coefficients(Uncontrolled)[["Time"]] * 107:211 + 
        mean(SDIL$AverageMonthlyTemperature[1:211])* coefficients(Uncontrolled)[["AverageMonthlyTemperature"]] +  
        mean(SDIL$December[1:211])* coefficients(Uncontrolled)[["December"]] +  
        mean(SDIL$January[1:211])* coefficients(Uncontrolled)[["January"]],
      col="red",lwd=2, lty="dashed")

###############################################################################
#POST ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[107:211],
      y=coefficients(Uncontrolled)[["(Intercept)"]] + 
        coefficients(Uncontrolled)[["Time"]] * 107:211 + 
        coefficients(Uncontrolled)[["Level"]]  + 
        coefficients(Uncontrolled)[["Trend"]] * 1:105 +  
        coefficients(Uncontrolled)[["I(Trend * Trend)"]] * (SDIL$Trend[107:211]^2) +
        mean(SDIL$AverageMonthlyTemperature[1:211]) * coefficients(Uncontrolled)[["AverageMonthlyTemperature"]] +  
        mean(SDIL$December[1:211]) * coefficients(Uncontrolled)[["December"]] +  
        mean(SDIL$January[1:211]) * coefficients(Uncontrolled)[["January"]], 
      col="red",lwd=2)

###############################################################################
#ADD ANNOUNCEMENT INDICATOR
###############################################################################

rect(16875,0,16880,8, col = "grey70", border=NA)

###############################################################################
#ADD LEGEND
###############################################################################

legend("topright", c("Controlled", "Uncontrolled","Toiletries"), 
       col=c("goldenrod","red","gray40"), lty=c(1, 1, 1), bty="n", lwd=2, cex=1, seg.len=3)

text(17320, 0.320, paste("Level (Controlled) =", round(coef(Controlled)[["IndicatorLevel"]],4), 
                         "\nLevel (Toiletries) =", round(coef(Controlled)[["Level"]],4), 
                           "\nLevel (Uncontrolled) =", round(coef(Uncontrolled)[["Level"]],4),
                           "\n\nTrend (Controlled)  =", round(coef(Controlled)[["I(IndicatorTrend * IndicatorTrend)"]],5), 
                     "\nTrend (Uncontrolled) =", round(coef(Uncontrolled)[["I(Trend * Trend)"]],5)), pos=4)

