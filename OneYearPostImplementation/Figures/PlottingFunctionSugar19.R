###############################################################################
#PLOTTING FUNCTION
#INCLUDES 8 PARTS, ONE CREATING A DATA FRAME CONTAINING THE EMPIRICAL VALUES
#A SECOND CONTAINING A FUNCTION TO AVERAGE OUT THE TERMS FOR DECEMBER, JANUARY
#EASTER AND AVERAGEMONTHLYTEMPERATURE. THE SUBSEQUENT 6 PARTS ARE USED TO PLOT
#SEGMENTS FOR THE CONTROL AND THE DRINK OF INTEREST
#THE FUNCTION TAKES 3 ARGUMENTS THE MODEL, A COLOUR ARGUMENT FOR THE LINE AND
#THE COEFFICIENT OF THE EXTRA TERMS
###############################################################################
#LIBRARIES
###############################################################################

library(plyr)

###############################################################################
#PLOT FUNCTION
###############################################################################

plotSugar.fun <- function(model, colour, coefficient, name, n=1, SndYAxisTo=0.25, SndYAxisLength=6, YAxisLimit=1.1){
 
###############################################################################
#STRIP "SUGAR" FROM MODEL NAME
###############################################################################
  
model <- ifelse(model =="LowSugar" |
                model =="NoSugar",model, sub("Sugar", "", model))

###############################################################################
#SELECT DATA
###############################################################################

Data <-  WeeklyVolume[WeeklyVolume$SDIL2 %in% c(model,"Toiletries")]

###############################################################################
#ORDER DATA
###############################################################################

Data <- Data[order(Data$SDIL2 %in% model, decreasing=TRUE)]

################################################################################
#SCALE DATA 
#THIS IS USED TO REDUCE THE UNITS TO KG OR 100G 
#THE DEFAULT IS 1 WHICH GIVES NO SCALING
###############################################################################

Data$sugar <- ifelse(Data$SDIL2 != "Toiletries",(Data$sugar/n),Data$sugar)

###############################################################################
#CALCULATE OFFSET FOR EXTRA VARIABLES 
#THIS FUNCTION TAKES THE COEFFICENT OF INTEREST AS AN ARGUMENT 
#CREATES A DATA FRAME CONTAINING THE MODEL COEFFICIENTS, SEARCHES TO SEE IF THE
#COEFFICIENT OF INTEREST IS INCLUDED IN THE ROW NAMES, AND, USING THE IFELSE FUNCTION,
#IF IT ISN'T INCLUDED RETURNS A 0, IN EFFECT CREATING NO OFFSET. OTHERWISE IF
#THE COEFFICIENT DOES EXIST THE MEAN OVER THE DATA IS DETERMINED AND MULTIPLIED
#BY THE MODEL COEFFICIENT TO CREATE AN OFFSET FOR EACH TIME POINT. THE FUNCTIONS 
#EVAL(PARSE...) ARE USED TO CONVERT THE OBJECT "COEFFICIENT" INTO A VARIABLE NAME
#THAT CAN BE SEARCHED
#
#ONLY FOR INTERVENTION GROUP - NOT CONTROL HENCE 212
#
#INCLUDES THE MEAN ACROSS THE DATA
#
###############################################################################
  
Offset <- function(coefficient){
  ifelse(sum(row.names(data.frame(unlist(coef(model)))) 
             %in% coefficient)==0,0,(sum(eval(parse(text=paste("Data$",coefficient,sep=""))))/nrow(Data[Data$SDIL2 !="Toiletries"]))
         * coef(model)[[coefficient]])
}

###############################################################################
#QUADRATIC COEFFICIENT (QC) FUNCTION
###############################################################################
  
QC.fun <- function(coefficient){
  ifelse(sum(row.names(data.frame(unlist(coefficients(model)))) 
             %in% coefficient)==0,0,coefficients(model)[[coefficient]])
}

###############################################################################
#SET PARAMETERS AROUND PLOTS INCLUDING COLOUR AND MARGINS
###############################################################################

# op <- par(mar = c(5,4,4,4) + 0.1)
# par(bg="white", op)
    
###############################################################################
#START WITH BLANK PLOT
###############################################################################
  
plot(Data$Weeks,Data$sugar, ylim=c(0,round(max(Data$sugar)*YAxisLimit,3)), 
     col="white",  xlab="", ,ylab="",yaxt="n", xaxt="n",yaxs="i")

###############################################################################
#ADD X AXIS LABELS
###############################################################################
  
axis.Date(side = 1, at = seq(from = min(Data$Weeks), to = max(Data$Weeks), 
                             length.out=length(unique(Data$Weeks))/52),
          format = "%b-%y", cex.axis=1.2)

###############################################################################
#ADD FIRST Y AXIS VALUES
###############################################################################

axis(side = 2, at = seq(from = 0, to = SndYAxisTo, length.out=SndYAxisLength),
     labels=seq(from = 0, to = SndYAxisTo,length.out=SndYAxisLength)*n)

###############################################################################
#ADD SECOND Y AXIS LABELS
###############################################################################

mtext("Mean Weekly Purchased Household Sugar (g)", side = 2, line=2.5)

###############################################################################
#ADD SECOND Y AXIS VALUES
###############################################################################

axis(side = 4, at = seq(from = 0, to = SndYAxisTo, length.out=SndYAxisLength),
     labels=seq(from = 0, to = SndYAxisTo*1000,length.out=SndYAxisLength))

###############################################################################
#ADD SECOND Y AXIS LABELS
###############################################################################

mtext("Mean Weekly Purchased Household Toiletries Volume (ml)", side = 4, line=2.5)

###############################################################################
#ADD DATA POINTS
###############################################################################

points(x=Data[Data$SDIL2 != "Toiletries"]$Weeks, y=Data[Data$SDIL2 != "Toiletries"]$sugar, col=adjustcolor(colour, alpha.f = 0.2),lwd=2, pch=3, cex=0.5)
points(x=Data[Data$SDIL2 == "Toiletries"]$Weeks, y=Data[Data$SDIL2 == "Toiletries"]$sugar, col="gray85",lwd=2, pch=1, cex=0.5)  

###############################################################################
#MODEL TO OBJECT
###############################################################################
  
model <- eval(parse(text=paste0(model, "Sugar")))

###############################################################################
#THE BELOW ADDS PLOTS THE REGRESSION COEFFICIENTS FOR BOTH THE INTERVENTION CONDITION
#AND THE CONTROL CONDITION. EACH CONDITION HAS THREE LINES HENCE 2X3 CHUNKS OF CODE
#PRE ANNOUNCEMENT TREND, POST ANNOUNCEMENT TREND AND COUNTERFACTUAL. THAT IS A 
#CONTINUATION OF THE PRE ANNOUNCEMENT TREND EXCLUDING THE IMPACT OF THE POST ANNOUNCEMENT
#TREND. LINES ARE PLOTTED USING THE SEGMENT FUNCTION WHICH REQUIRES 4 ARGUMENTS.
#X AND Y COORDINATES FOR BOTH THE START AND END OF THE LINE.
#X FOR THE START OF THE LINE IS THEREFORE SPECIFIED BY THE FIRST WEEK OF THE TIME  
#PERIOD - WEEK 1 (2014-03-09) FOR THE PRE ANNOUNCEMENT PERIOD AND WEEK 107 (2016-03-21)
#FOR THE FIRST WEEK IN THE POST ANNOUNCEMENT PERIOD. SIMILARLY X1 INDICATES THE FINAL
#WEEK OF THE PERIOD OF INTEREST WHICH IS WEEK 106 (2016-03-14) FOR THE PRE ANNOUNCEMENT
#PERIOD AND WEEK 212 (2018-03-25) FOR THE FINAL WEEK IN THE POST ANNOUNCEMENT PERIOD
#-----------------------------------------------------------------------------#
#THE Y POSITION VARIES IN MOST CASES. IN THE PRE ANNOUNCEMENT CONTROL (TOILETRIES) LINE
#Y0 IS GIVEN BY THE INTERCEPT + TIME*1 + THE OFFSETS FOR EACH OF THE 5 COVARIATES
#Y1 IS GIVEN BY THE INTERCEPT + TIME*106 (AS THIS IS THE NUMBER OF WEEKS) + THE
#OFFSETS FOR THE 5 COVARIATES
###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 0 & Data$SDILImplementation == 0]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Time"]]                       * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 0 & Data$SDILImplementation == 0]$Time + +
        coef(model)[["AverageMonthlyTemperature"]]  * mean(Data[Data$SDIL2 == "Toiletries"]$AverageMonthlyTemperature) +  
        Offset("December")                          + 
        Offset("January")                           + 
        Offset("Easter"))/n, 
      col="gray40",lwd=2)

###############################################################################
#PRE IMPLEMENTATION REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Time"]]                       * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$Time +
        coef(model)[["SDILAnnouncement"]]           * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$SDILAnnouncement + 
        coef(model)[["SDILImplementation"]]         * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$SDILImplementation + 
        coef(model)[["AnnouncementTrend"]]          * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementTrend +
        coef(model)[["ImplementationTrend"]]        * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$ImplementationTrend +
        coef(model)[["AverageMonthlyTemperature"]]  * mean(Data[Data$SDIL2 == "Toiletries"]$AverageMonthlyTemperature) +
        Offset("December")                          + 
        Offset("January")                           + 
        Offset("Easter"))/n, 
      col="gray40",lwd=2)

###############################################################################
#POST IMPLEMENTATION REGRESSION LINE - TOILETRIES
###############################################################################

lines(x=Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Time"]]                       * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$Time + 
        coef(model)[["SDILAnnouncement"]]           * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$SDILAnnouncement + 
        coef(model)[["SDILImplementation"]]         * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$SDILImplementation + 
        coef(model)[["AnnouncementTrend"]]          * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementTrend +
        coef(model)[["ImplementationTrend"]]        * Data[Data$SDIL2 == "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationTrend +
        coef(model)[["AverageMonthlyTemperature"]]  * mean(Data[Data$SDIL2 == "Toiletries"]$AverageMonthlyTemperature) + 
        Offset("December")                          + 
        Offset("January")                           + 
        Offset("Easter"))/n, 
      col="gray40",lwd=2)

###############################################################################
#PRE ANNOUNCEMENT REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 0 & Data$SDILImplementation == 0]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Indicator"]]                  * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 0 & Data$SDILImplementation == 0]$Indicator +
        coef(model)[["Time"]]                       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 0 & Data$SDILImplementation == 0]$Time + 
        coef(model)[["IndicatorTime"]]              * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 0 & Data$SDILImplementation == 0]$IndicatorTime + 
        coef(model)[["AverageMonthlyTemperature"]]  * mean(Data[Data$SDIL2 != "Toiletries"]$AverageMonthlyTemperature) +  
        Offset("December")                          + 
        Offset("January")                           + 
        Offset("Easter"))/n, 
      col=colour,lwd=2)

###############################################################################
#PRE IMPLEMENTATION REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Indicator"]]                   * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$Indicator + 
        coef(model)[["Time"]]                        * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$Time + 
        coef(model)[["IndicatorTime"]]               * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$IndicatorTime + 
        coef(model)[["SDILAnnouncement"]]            * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$SDILAnnouncement + 
        coef(model)[["AnnouncementIndicatorLevel"]]  * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementIndicatorLevel +  
        coef(model)[["AnnouncementTrend"]]           * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementTrend +  
        coef(model)[["AnnouncementIndicatorTrend"]]  * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementIndicatorTrend +  
        QC.fun("I(AnnouncementIndicatorTrend^2)")    * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementIndicatorTrend^2 +
        QC.fun("I(AnnouncementIndicatorTrend^3)")    * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementIndicatorTrend^3 +
        QC.fun("I(AnnouncementIndicatorTrend^4)")    * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 0]$AnnouncementIndicatorTrend^4 +
        coef(model)[["AverageMonthlyTemperature"]]   * mean(Data[Data$SDIL2 != "Toiletries"]$AverageMonthlyTemperature) + 
        Offset("December")                           + 
        Offset("January")                            + 
        Offset("Easter"))/n,
      col=colour,lwd=2)

###############################################################################
#POST IMPLEMENTATION REGRESSION LINE - INTERVENTION
###############################################################################

lines(x=Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Indicator"]]                        * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$Indicator + 
        coef(model)[["Time"]]                             * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$Time +
        coef(model)[["IndicatorTime"]]                    * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$IndicatorTime + 
        coef(model)[["SDILAnnouncement"]]                 * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$SDILAnnouncement + 
        coef(model)[["SDILImplementation"]]               * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$SDILImplementation + 
        coef(model)[["AnnouncementIndicatorLevel"]]       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementIndicatorLevel + 
        coef(model)[["ImplementationIndicatorLevel"]]     * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationIndicatorLevel + 
        coef(model)[["AnnouncementTrend"]]                * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementTrend +  
        coef(model)[["ImplementationTrend"]]              * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationTrend +  
        coef(model)[["AnnouncementIndicatorTrend"]]       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend + 
        coef(model)[["ImplementationIndicatorTrend"]]     * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationIndicatorTrend + 
        QC.fun("I(AnnouncementIndicatorTrend^2)")         * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend^2 +
        QC.fun("I(AnnouncementIndicatorTrend^3)")         * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend^3 +
        QC.fun("I(AnnouncementIndicatorTrend^4)")         * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend^4 +
        QC.fun("I(ImplementationIndicatorTrend^2)")       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationIndicatorTrend^2 +
        QC.fun("I(ImplementationIndicatorTrend^3)")       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationIndicatorTrend^3 +
        QC.fun("I(ImplementationIndicatorTrend^4)")       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 & Data$SDILImplementation == 1]$ImplementationIndicatorTrend^4 +
        coef(model)[["AverageMonthlyTemperature"]]        * mean(Data[Data$SDIL2 != "Toiletries"]$AverageMonthlyTemperature) + 
        Offset("December")                                + 
        Offset("January")                                 + 
        Offset("Easter"))/n, 
      col=colour,lwd=2)

###############################################################################
#PRE IMPLEMENTATION COUNTERFACTUAL - INTERVENTION
###############################################################################

lines(x=Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Indicator"]]                  * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 ]$Indicator +
        coef(model)[["Time"]]                       * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 ]$Time + 
        coef(model)[["IndicatorTime"]]              * Data[Data$SDIL2 != "Toiletries" & Data$SDILAnnouncement == 1 ]$IndicatorTime + 
        coef(model)[["AverageMonthlyTemperature"]]  * mean(Data[Data$SDIL2 != "Toiletries"]$AverageMonthlyTemperature) +  
        Offset("December")                          + 
        Offset("January")                           + 
        Offset("Easter"))/n, 
      col=colour,lwd=2, lty="dashed")

###############################################################################
#POST IMPLEMENTATION COUNTERFACTUAL - INTERVENTION
###############################################################################

lines(x=Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$Weeks,
      y=(coef(model)[["(Intercept)"]] + 
        coef(model)[["Indicator"]]                   * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$Indicator + 
        coef(model)[["Time"]]                        * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$Time + 
        coef(model)[["IndicatorTime"]]               * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$IndicatorTime + 
        coef(model)[["SDILAnnouncement"]]            * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$SDILAnnouncement + 
        coef(model)[["AnnouncementIndicatorLevel"]]  * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$AnnouncementIndicatorLevel +  
        coef(model)[["AnnouncementTrend"]]           * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$AnnouncementTrend +  
        coef(model)[["AnnouncementIndicatorTrend"]]  * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend +  
        QC.fun("I(AnnouncementIndicatorTrend^2)")    * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend^2 +
        QC.fun("I(AnnouncementIndicatorTrend^3)")    * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend^3 +
        QC.fun("I(AnnouncementIndicatorTrend^4)")    * Data[Data$SDIL2 != "Toiletries" & Data$SDILImplementation == 1]$AnnouncementIndicatorTrend^4 +
        coef(model)[["AverageMonthlyTemperature"]]   * mean(Data[Data$SDIL2 != "Toiletries"]$AverageMonthlyTemperature) + 
        Offset("December")                           + 
        Offset("January")                            + 
        Offset("Easter"))/n,
      col=colour,lwd=2, lty="dotted")

###############################################################################
#ADD ANNOUNCEMENT INDICATOR
###############################################################################

rect(16875,-0.5,16880,8, col = "grey70", border=NA)

###############################################################################
#ADD IMPLEMENTATION INDICATOR
###############################################################################

rect(17625,-0.5,17630,8, col = "grey70", border=NA)

###############################################################################
#ADD LEGEND
###############################################################################

# legend("topright", c(name, "Counterfactual","Control (toiletries)"), 
#        col=c(colour,colour,"gray40"), lty=c(1, 2, 1), bty="n", lwd=2, cex=1, seg.len=3)
}
