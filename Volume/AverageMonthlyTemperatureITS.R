###############################################################################
#BOTTLED WATER ITS REGRESSION
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

SDIL <- WeeklyVolume[WeeklyVolume$SDIL2=="HigherTier"]

###############################################################################
#EXTRACT TOILETRIES CONTROL CONDITION
###############################################################################

Control <- WeeklyVolume[WeeklyVolume$SDIL2=="Toiletries"]

###############################################################################
#REPLACE SUGAR NA WITH VOLUME
###############################################################################

Control$sugar <- Control$volume

###############################################################################
#CREATE INTERVENTION (NON-CONTROL) CONDITION IDENTIFIER
###############################################################################

SDIL$Indicator <- 1

###############################################################################
#CREATE INTERVENTION (NON-CONTROL) CONDITION IDENTIFIER
###############################################################################

Control$Indicator <- 0

###############################################################################
#BIND TOGETHER
###############################################################################

SDIL <- rbind(SDIL,Control)

###############################################################################
#ADD TIME INDICATOR 
###############################################################################

SDIL$IndicatorTime <- c(seq(1:212),rep(0,212))

###############################################################################
#ADD LEVEL INDICATOR 
###############################################################################

SDIL$IndicatorLevel <- c(rep(0,107), rep(1,105),rep(0,212))

###############################################################################
#ADD TREND INDICATOR 
###############################################################################

SDIL$IndicatorTrend <- c(rep(0,107), seq(1:105),rep(0,212))

###############################################################################
#REMOVE AVERAGE MONTHLY TEMPERATURE VALUE
###############################################################################

# SDIL$AverageMonthlyTemperature <- ifelse(SDIL$SDIL2=="Toiletries",0,SDIL$AverageMonthlyTemperature)

###############################################################################
#REMOVE CONTROL
###############################################################################

rm(Control)

###############################################################################
#UPDATE TO MODEL OF BEST FIT
###############################################################################

summary(lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
             I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
             AverageMonthlyTemperature + December + Easter + 
             January, data=SDIL))

SDIL$AverageMonthlyTemperature <- ifelse(SDIL$SDIL2=="Toiletries",0,SDIL$AverageMonthlyTemperature)

summary(lm(volume ~ Time + Indicator + IndicatorTime + Level + Trend + 
             I(IndicatorTrend*IndicatorTrend) + IndicatorLevel + IndicatorTrend + 
             AverageMonthlyTemperature + December + Easter + 
             January, data=SDIL))
