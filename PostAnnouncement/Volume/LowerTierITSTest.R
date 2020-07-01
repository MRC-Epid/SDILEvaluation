###############################################################################
#LOWER TIER ITS REGRESSION
#DAVID PELL
#15/01/19
###############################################################################
#OPEN DATA
###############################################################################

if(!exists("WeeklyVolume")){
  source("/home/dap49/KWPITS/ITSData.R")
}

###############################################################################
#OFFSET COEFFICIENT (QC) FUNCTION
###############################################################################

Offset <- function(coefficient){
  ifelse(sum(row.names(data.frame(unlist(coefficients(model)))) 
             %in% coefficient)==0,0,(sum(eval(parse(text=paste("Data$",coefficient,sep=""))))/212)
         * coefficients(model)[[coefficient]])
}

###############################################################################
#QUADRATIC COEFFICIENT (QC) FUNCTION
###############################################################################

QC.fun <- function(coefficient){
  ifelse(sum(row.names(data.frame(unlist(coefficients(model)))) 
             %in% coefficient)==0,0,coefficients(model)[[coefficient]])
}

###############################################################################
#SUBSET WEEKLYVOLUME TO CATEGORY OF INTEREST
###############################################################################

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="LowerTier"]
Data <- SDIL

###############################################################################
#ADD CONTROL AND INTERACTION VARIABLES
###############################################################################

source("/home/dap49/KWPITS/ControlDataSetup.R")

###############################################################################
#CREATE VARIABLE FOR PRE-INTERVENTION MODELING
###############################################################################

SDIL$PreTime <- c(seq(1:107),rep(0,317))

##############################################################################
#EMPIRICAL PLOT
###############################################################################

ggplot(SDIL,aes(x=Weeks,y=volume, colour=SDIL2)) +
  geom_line() + 
  geom_point() + 
  geom_vline(xintercept=as.Date("2016-03-16")) +
  geom_smooth()

###############################################################################
#MODEL
###############################################################################

model <- gls(volume ~ Time + Indicator + IndicatorTime + Level + Trend + I(IndicatorTrend*IndicatorTrend) + 
               IndicatorLevel + IndicatorTrend + AverageMonthlyTemperature + December + January, 
             data=SDIL, correlation=corARMA(p=1,form=~Time|Indicator),method="ML")

###############################################################################
#START WITH BLANK PLOT
###############################################################################

plot(SDIL$Weeks,SDIL$volume, ylim=c(0,round(max(SDIL$volume)*1.02,1)), 
     col="white", ylab="Mean Weekly Purchased Household Volume (L/kg)", xlab="", yaxs="i",
     xaxt="n")

###############################################################################
#ADD X AXIS LABELS
###############################################################################

axis.Date(side = 1, at = seq(from = min(SDIL$Weeks), to = max(SDIL$Weeks), 
                             length.out=9), format = "%b-%y", cex.axis=0.8)

###############################################################################
#ADD DATA POINTS
###############################################################################

points(x=SDIL$Weeks[1:212], y=SDIL$volume[1:212], col="gray70",lwd=2, pch=20, cex=0.5)
points(x=SDIL$Weeks[1:212], y=SDIL$volume[213:424], col="gray70",lwd=2, pch=4, cex=0.5)

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=SDIL$Weeks[1:107],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Time"]] * 1:107 + 
        mean(SDIL$AverageMonthlyTemperature[1:212])* coefficients(model)[["AverageMonthlyTemperature"]],
      col="gray40",lwd=2)

###############################################################################
#COUNTERFACTUAL - TOILETRIES
###############################################################################

lines(x=SDIL$Weeks[108:212],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Time"]] * 108:212 + 
        coefficients(model)[["Trend"]] * 1:105 + 
        mean(SDIL$AverageMonthlyTemperature[1:212])* coefficients(model)[["AverageMonthlyTemperature"]], 
      lty="dashed",col="gray40",lwd=2)

###############################################################################
#POST ANNOUNCEMENT REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=SDIL$Weeks[108:212],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Time"]] * 108:212 + 
        coefficients(model)[["Level"]]  + 
        coefficients(model)[["Trend"]] * 1:105 + 
        mean(SDIL$AverageMonthlyTemperature[1:212])* coefficients(model)[["AverageMonthlyTemperature"]], 
      col="gray40",lwd=2)

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[1:107],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] +
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 1:107 + 
        mean(SDIL$AverageMonthlyTemperature[1:212])* coefficients(model)[["AverageMonthlyTemperature"]] +  
        Offset("December") + 
        Offset("January") + 
        Offset("Easter"), 
      col="red",lwd=2)

###############################################################################
#COUNTERFACTUAL - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[108:212],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] + 
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 108:212 + 
        coefficients(model)[["Level"]]  + 
        coefficients(model)[["Trend"]] * 1:105 + 
        mean(SDIL$AverageMonthlyTemperature[1:212])* coefficients(model)[["AverageMonthlyTemperature"]] + 
        Offset("December") + 
        Offset("January") + 
        Offset("Easter"), 
      col="red",lwd=2, lty="dashed")

###############################################################################
#POST ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=SDIL$Weeks[108:212],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] + 
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 108:212 + 
        coefficients(model)[["Level"]]  + 
        coefficients(model)[["IndicatorLevel"]] + 
        (coefficients(model)[["Trend"]] + coefficients(model)[["IndicatorTrend"]]) * 1:105 + 
        QC.fun("I(IndicatorTrend * IndicatorTrend)") * (SDIL$IndicatorTrend[108:212]^2) +
        mean(SDIL$AverageMonthlyTemperature[1:212])* coefficients(model)[["AverageMonthlyTemperature"]] + 
        Offset("December") + 
        Offset("January") + 
        Offset("Easter"), col="red",lwd=2)

###############################################################################
#ADD ANNOUNCEMENT INDICATOR
###############################################################################

rect(16875,0,16880,8, col = "grey70", border=NA)

###############################################################################
#ADD FITTED LINES
###############################################################################

lines(x=SDIL$Weeks[1:212],y=fitted(model)[1:212],col="red")
lines(x=SDIL$Weeks[1:212],y=fitted(model)[213:424])

###############################################################################
#START WITH BLANK PLOT
###############################################################################

plot(SDIL$Weeks,SDIL$volume, ylim=c(0,round(max(SDIL$volume)*1.02,1)), 
     col="white", ylab="Mean Weekly Purchased Household Volume (L/kg)", xlab="", yaxs="i",
     xaxt="n")

###############################################################################
#ADD X AXIS LABELS
###############################################################################

axis.Date(side = 1, at = seq(from = min(SDIL$Weeks), to = max(SDIL$Weeks), 
                             length.out=9), format = "%b-%y", cex.axis=0.8)

###############################################################################
#ADD DATA POINTS
###############################################################################

points(x=SDIL$Weeks[1:212], y=SDIL$volume[1:212], col="gray70",lwd=2, pch=20, cex=0.5)
points(x=SDIL$Weeks[1:212], y=SDIL$volume[213:424], col="gray70",lwd=2, pch=4, cex=0.5)

###############################################################################
#INCLUDE NON LINEAR TERM IN PRE ANNOUNCEMENT PERIOD
###############################################################################

model <- gls(volume ~ Time + Indicator + IndicatorTime + poly(IndicatorTime,3,raw=TRUE) + Level + Trend + I(IndicatorTrend*IndicatorTrend) + 
               IndicatorLevel + IndicatorTrend, data=SDIL, correlation=corARMA(p=1,form=~Time|Indicator),method="ML")

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - INTERVENTION -INCLUDING NON LINEAR TERM
###############################################################################

lines(x=SDIL$Weeks[1:107],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] +
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 1:107 + 
           (coefficients(model)[["I(IndicatorTime^2)"]]) * (SDIL$IndicatorTime[1:107]^2) + 
        (coefficients(model)[["I(IndicatorTime^3)"]]) * (SDIL$IndicatorTime[1:107]^3), 
      col="red",lwd=2)

###############################################################################
#COUNTERFACTUAL - INTERVENTION - INCLUDING NON LINEAR TERM
###############################################################################

lines(x=SDIL$Weeks[108:212],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] + 
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 108:212 + 
        (coefficients(model)[["I(IndicatorTime^2)"]]) * (SDIL$IndicatorTime[108:212]^2)  +
        (coefficients(model)[["I(IndicatorTime^3)"]]) * (SDIL$IndicatorTime[108]^3) + 
        coefficients(model)[["Level"]]  + 
        coefficients(model)[["Trend"]] * 1:105,  
      col="red",lwd=2, lty="dashed")

###############################################################################
#ADD FITTED LINES
###############################################################################

lines(x=SDIL$Weeks[1:212],y=fitted(model)[1:212],col="red")
lines(x=SDIL$Weeks[1:212],y=fitted(model)[213:424])

###############################################################################
#INCLUDE NON LINEAR TERM IN PRE ANNOUNCEMENT PERIOD
###############################################################################

model <- gls(volume ~ Time + Indicator + IndicatorTime + poly(PreTime,3,raw=TRUE) + Level + Trend + I(IndicatorTrend*IndicatorTrend) + 
               IndicatorLevel + IndicatorTrend, data=SDIL, correlation=corARMA(p=1,form=~Time|Indicator),method="ML")

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - INTERVENTION -INCLUDING NON LINEAR TERM
###############################################################################

lines(x=SDIL$Weeks[1:107],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] +
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 1:107 + 
        (coefficients(model)[["poly(PreTime, 3, raw = TRUE)3 "]]) * (poly(SDIL$PreTime[1:107]^3), 
      col="red",lwd=2)

###############################################################################
#COUNTERFACTUAL - INTERVENTION - INCLUDING NON LINEAR TERM
###############################################################################

lines(x=SDIL$Weeks[108:212],
      y=coefficients(model)[["(Intercept)"]] + 
        coefficients(model)[["Indicator"]] + 
        (coefficients(model)[["Time"]] + coefficients(model)[["IndicatorTime"]]) * 108:212 + 
        (coefficients(model)[["I(IndicatorTime^2)"]]) * (SDIL$IndicatorTime[108:212]^2)  +
        (coefficients(model)[["I(IndicatorTime^3)"]]) * (SDIL$IndicatorTime[108]^3) + 
        coefficients(model)[["Level"]]  + 
        coefficients(model)[["Trend"]] * 1:105,  
      col="red",lwd=2, lty="dashed")

###############################################################################
#ADD FITTED LINES
###############################################################################

lines(x=SDIL$Weeks[1:212],y=fitted(model)[1:212],col="red")
lines(x=SDIL$Weeks[1:212],y=fitted(model)[213:424])
