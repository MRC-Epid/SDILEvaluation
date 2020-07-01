###############################################################################
#CALCULATE STANDARD ERRORS FOR IMPLEMENTATION
#DAVID PELL
#22/01/2020
###############################################################################
#IMPLEMENTATION ABSOLUTE STANDARD ERROR ONLY INCLUDES THE FOLLOWING TERMS: 
#SDILImplmentation
#ImplementationTrend
#ImplementationIndicatorLevel
#ImplementationIndicatorTrend
###############################################################################
#FUNCTION FOR ABSOLUTE DIFFERENCES 
#ONE FUNCTION.DIFFERENT FROM PREVIOUS VERSION AS IT CHECKS FOR A QUADRATIC TERM
#SECOND FUNCTION FOR POST-ANNOUNCEMENT TO POST-IMPLEMENTATION
###############################################################################

AbsoluteChangeILowerTier.fun <-function(model, n=1, TimePointI1=264, TimePointI2=257){

###############################################################################
#COPY MODEL NAME
###############################################################################

modelName <- model  

###############################################################################
#STRIP "SUGAR" FROM MODEL NAME
###############################################################################

model <- ifelse(model =="LowSugar" |
                model =="NoSugar",model, sub("Sugar", "", model))  

###############################################################################
#STRIP SENSITIVITY ANALYSES INDICATORS FROM MODEL NAME
###############################################################################

model <- sub("05M","", model)
model <- sub("1M", "", model)

###############################################################################
#QUADRATIC COEFFICIENT (QC) FUNCTION
###############################################################################

QC.fun <- function(coefficient){
  ifelse(sum(names(coef(model))
             %in% coefficient)==0,0,coefficients(model)[[coefficient]])
}

###############################################################################
#CORRELATION COEFFICIENT (CC) FUNCTION
###############################################################################

CC.fun <- function(coefficient){
  ifelse(sum(names(coef(model)) %in% coefficient)==0,0,vcov(model)[[coefficient,coefficient]])
}

###############################################################################
#VARIANCE CORRELATION (VC) FUNCTION
###############################################################################

VC.fun <- function(Term,PowerTerm){
  Result <- ifelse(sum(names(coef(model)) %in% PowerTerm)==0,0,vcov(model)[,Term][PowerTerm])
  return(Result)
}

###############################################################################
#SELECT DATA
###############################################################################

Data <-  WeeklyVolume[WeeklyVolume$SDIL2 %in% c(model,"Toiletries")]

###############################################################################
#ORDER DATA
###############################################################################

Data <- Data[order(Data$SDIL2 %in% "Toiletries", decreasing=FALSE)]

################################################################################
#SCALE DATA 
#THIS IS USED TO REDUCE THE UNITS TO KG OR 100G 
###############################################################################

Data$sugar <- ifelse(Data$SDIL2 != "Toiletries",(Data$sugar/n),Data$sugar)

###############################################################################
#MODEL TO OBJECT
###############################################################################

model <- eval(parse(text=paste0(modelName)))

###############################################################################
#MODEL COVARIATES LIST
###############################################################################

covariates <- c("(Intercept)",                       #1
                "Indicator",                         #2    
                "Time",                              #3
                "IndicatorTime",                     #4
                "SDILAnnouncement",                  #5
                "AnnouncementIndicatorLevel",        #6
                "AnnouncementTrend",                 #7
                "AnnouncementIndicatorTrend",        #8
                "AverageMonthlyTemperature",         #9
                "December",                          #10
                "January",                           #11
                "SDILImplementation",                #12
                "ImplementationIndicatorLevel",      #13
                "ImplementationTrend",               #14
                "ImplementationIndicatorTrend",      #15
                "I(AnnouncementIndicatorTrend^2)")   #16

###############################################################################
#WITH HAS ALL THE TERMS
###############################################################################

YWith = 
  coef(model)[[covariates[1]]]  + 
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointI1] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointI1] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointI1] +
  coef(model)[[covariates[5]]]  * eval(parse(text=paste0("Data$",covariates[5])))[TimePointI1] +
  coef(model)[[covariates[6]]]  * eval(parse(text=paste0("Data$",covariates[6])))[TimePointI1] +  
  coef(model)[[covariates[7]]]  * eval(parse(text=paste0("Data$",covariates[7])))[TimePointI1] +
  coef(model)[[covariates[8]]]  * eval(parse(text=paste0("Data$",covariates[8])))[TimePointI1] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))) +
  coef(model)[[covariates[12]]] * eval(parse(text=paste0("Data$",covariates[12])))[TimePointI1] +
  coef(model)[[covariates[13]]] * eval(parse(text=paste0("Data$",covariates[13])))[TimePointI1] +
  coef(model)[[covariates[14]]] * eval(parse(text=paste0("Data$",covariates[14])))[TimePointI1] +
  coef(model)[[covariates[15]]] * eval(parse(text=paste0("Data$",covariates[15])))[TimePointI1] +
  coef(model)[[covariates[16]]] * Data$AnnouncementIndicatorTrend[TimePointI1]^2

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS - COUNTERFACTUAL
###############################################################################

YWithout =  
  coef(model)[[covariates[1]]]  + 
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointI2] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointI2] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointI2] +
  coef(model)[[covariates[5]]]  * eval(parse(text=paste0("Data$",covariates[5])))[TimePointI2] +
  coef(model)[[covariates[6]]]  * eval(parse(text=paste0("Data$",covariates[6])))[TimePointI2] +  
  coef(model)[[covariates[7]]]  * eval(parse(text=paste0("Data$",covariates[7])))[TimePointI2] +
  coef(model)[[covariates[8]]]  * eval(parse(text=paste0("Data$",covariates[8])))[TimePointI2] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")")))  + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))) +
  #   coef(model)[[covariates[12]]]  * eval(parse(text=paste0("Data$",covariates[12])))[TimePointI2] +
  #   coef(model)[[covariates[13]]]  * eval(parse(text=paste0("Data$",covariates[13])))[TimePointI2] +
  #   coef(model)[[covariates[14]]]  * eval(parse(text=paste0("Data$",covariates[14])))[TimePointI2] +
  #   coef(model)[[covariates[15]]]  * eval(parse(text=paste0("Data$",covariates[15])))[TimePointI2]
  coef(model)[[covariates[16]]]  * Data$AnnouncementIndicatorTrend[TimePointI2]^2


###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION
###############################################################################

AbsDiffImplementation <- YWith - YWithout

###############################################################################
#DETERMINE VARIANCE FOR FULL MODEL
###############################################################################

VarWith <- 
  (vcov(model)["(Intercept)","(Intercept)"] + 
     vcov(model)["Time","Time"] * Data$Time ^ 2 +
     vcov(model)["Indicator","Indicator"] * Data$Indicator ^ 2 +
     vcov(model)["IndicatorTime","IndicatorTime"] * Data$IndicatorTime ^ 2 +
     vcov(model)["SDILAnnouncement","SDILAnnouncement"] * Data$SDILAnnouncement ^ 2 +
     vcov(model)["AnnouncementTrend","AnnouncementTrend"] * Data$AnnouncementTrend ^ 2 +
     vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorLevel"] * Data$AnnouncementIndicatorLevel ^ 2 +
     vcov(model)["AnnouncementIndicatorTrend","AnnouncementIndicatorTrend"] * Data$AnnouncementIndicatorTrend ^ 2 +
     vcov(model)["SDILImplementation","SDILImplementation"] * Data$SDILImplementation ^ 2 +
     vcov(model)["ImplementationTrend","ImplementationTrend"] * Data$ImplementationTrend ^ 2 +
     vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorLevel"] * Data$ImplementationIndicatorLevel ^ 2 +
     vcov(model)["ImplementationIndicatorTrend","ImplementationIndicatorTrend"] * Data$ImplementationIndicatorTrend ^ 2 +
     vcov(model)["I(AnnouncementIndicatorTrend^2)","I(AnnouncementIndicatorTrend^2)"] * Data$AnnouncementIndicatorTrend ^ 4 +
     vcov(model)["December","December"] * Data$December ^ 2 +
     vcov(model)["January","January"] * Data$January ^ 2 +
     vcov(model)["AverageMonthlyTemperature","AverageMonthlyTemperature"] * Data$AverageMonthlyTemperature ^ 2 +
     
     #COVARIANCE
     vcov(model)["(Intercept)","Time"] * 2 * Data$Time +
     vcov(model)["(Intercept)","Indicator"] * 2 * Data$Indicator +
     vcov(model)["(Intercept)","IndicatorTime"] * 2 * Data$IndicatorTime +
     vcov(model)["(Intercept)","SDILAnnouncement"] * 2 * Data$SDILAnnouncement +
     vcov(model)["(Intercept)","AnnouncementTrend"] * 2 * Data$AnnouncementTrend +
     vcov(model)["(Intercept)","AnnouncementIndicatorLevel"] * 2 * Data$AnnouncementIndicatorLevel +
     vcov(model)["(Intercept)","AnnouncementIndicatorTrend"] * 2 * Data$AnnouncementIndicatorTrend +
     vcov(model)["(Intercept)","SDILImplementation"] * 2 * Data$SDILImplementation +
     vcov(model)["(Intercept)","ImplementationTrend"] * 2 * Data$ImplementationTrend +
     vcov(model)["(Intercept)","ImplementationIndicatorLevel"] * 2 * Data$ImplementationIndicatorLevel +
     vcov(model)["(Intercept)","ImplementationIndicatorTrend"] * 2 * Data$ImplementationIndicatorTrend +
     vcov(model)["(Intercept)","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementIndicatorTrend^2 +
     vcov(model)["(Intercept)","December"] * 2 * Data$December +
     vcov(model)["(Intercept)","January"] * 2 * Data$January +
     vcov(model)["(Intercept)","AverageMonthlyTemperature"] * 2 * Data$AverageMonthlyTemperature +
     
     vcov(model)["Time","Indicator"] * 2 * Data$Time * Data$Indicator +
     vcov(model)["Time","IndicatorTime"] * 2 * Data$Time * Data$IndicatorTime +
     vcov(model)["Time","SDILAnnouncement"] * 2 * Data$Time * Data$SDILAnnouncement +
     vcov(model)["Time","AnnouncementTrend"] * 2 * Data$Time * Data$AnnouncementTrend +
     vcov(model)["Time","AnnouncementIndicatorLevel"] * 2 * Data$Time * Data$AnnouncementIndicatorLevel +
     vcov(model)["Time","AnnouncementIndicatorTrend"] * 2 * Data$Time * Data$AnnouncementIndicatorTrend +
     vcov(model)["Time","SDILImplementation"] * 2 * Data$Time * Data$SDILImplementation +
     vcov(model)["Time","ImplementationTrend"] * 2 * Data$Time * Data$ImplementationTrend +
     vcov(model)["Time","ImplementationIndicatorLevel"] * 2 * Data$Time * Data$ImplementationIndicatorLevel +
     vcov(model)["Time","ImplementationIndicatorTrend"] * 2 * Data$Time * Data$ImplementationIndicatorTrend +
     vcov(model)["Time","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$Time * Data$AnnouncementIndicatorTrend^2 +
     vcov(model)["Time","December"] * 2 * Data$Time * Data$December +
     vcov(model)["Time","January"] * 2 * Data$Time * Data$January +
     vcov(model)["Time","AverageMonthlyTemperature"] * 2 * Data$Time * Data$AverageMonthlyTemperature +
     
     vcov(model)["Indicator","IndicatorTime"] * 2 * Data$Indicator * Data$IndicatorTime + 
     vcov(model)["Indicator","SDILAnnouncement"] * 2 * Data$Indicator * Data$SDILAnnouncement +
     vcov(model)["Indicator","AnnouncementTrend"] * 2 * Data$Indicator * Data$AnnouncementTrend +
     vcov(model)["Indicator","AnnouncementIndicatorLevel"] * 2 * Data$Indicator * Data$AnnouncementIndicatorLevel +
     vcov(model)["Indicator","AnnouncementIndicatorTrend"] * 2 * Data$Indicator * Data$AnnouncementIndicatorTrend +
     vcov(model)["Indicator","SDILImplementation"] * 2 * Data$Indicator * Data$SDILImplementation +
     vcov(model)["Indicator","ImplementationTrend"] * 2 * Data$Indicator * Data$ImplementationTrend +
     vcov(model)["Indicator","ImplementationIndicatorLevel"] * 2 * Data$Indicator * Data$ImplementationIndicatorLevel +
     vcov(model)["Indicator","ImplementationIndicatorTrend"] * 2 * Data$Indicator * Data$ImplementationIndicatorTrend +
     vcov(model)["Indicator","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$Indicator * Data$AnnouncementIndicatorTrend^2 +
     vcov(model)["Indicator","December"] * 2 * Data$Indicator * Data$December +
     vcov(model)["Indicator","January"] * 2 * Data$Indicator * Data$January +
     vcov(model)["Indicator","AverageMonthlyTemperature"] * 2 * Data$Indicator * Data$AverageMonthlyTemperature +
     
     vcov(model)["IndicatorTime","SDILAnnouncement"] * 2 * Data$IndicatorTime * Data$SDILAnnouncement +
     vcov(model)["IndicatorTime","AnnouncementTrend"] * 2 * Data$IndicatorTime * Data$AnnouncementTrend + 
     vcov(model)["IndicatorTime","AnnouncementIndicatorLevel"] * 2 * Data$IndicatorTime * Data$AnnouncementIndicatorLevel + 
     vcov(model)["IndicatorTime","AnnouncementIndicatorTrend"] * 2 * Data$IndicatorTime * Data$AnnouncementIndicatorTrend + 
     vcov(model)["IndicatorTime","SDILImplementation"] * 2 * Data$IndicatorTime * Data$SDILImplementation +
     vcov(model)["IndicatorTime","ImplementationTrend"] * 2 * Data$IndicatorTime * Data$ImplementationTrend + 
     vcov(model)["IndicatorTime","ImplementationIndicatorLevel"] * 2 * Data$IndicatorTime * Data$ImplementationIndicatorLevel + 
     vcov(model)["IndicatorTime","ImplementationIndicatorTrend"] * 2 * Data$IndicatorTime * Data$ImplementationIndicatorTrend + 
     vcov(model)["IndicatorTime","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$IndicatorTime * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["IndicatorTime","December"] * 2 * Data$IndicatorTime * Data$December + 
     vcov(model)["IndicatorTime","January"] * 2 * Data$IndicatorTime * Data$January + 
     vcov(model)["IndicatorTime","AverageMonthlyTemperature"] * 2 * Data$IndicatorTime * Data$AverageMonthlyTemperature + 
     
     vcov(model)["SDILAnnouncement","AnnouncementTrend"] * 2 * Data$SDILAnnouncement * Data$AnnouncementTrend + 
     vcov(model)["SDILAnnouncement","AnnouncementIndicatorLevel"] * 2 * Data$SDILAnnouncement * Data$AnnouncementIndicatorLevel + 
     vcov(model)["SDILAnnouncement","AnnouncementIndicatorTrend"] * 2 * Data$SDILAnnouncement * Data$AnnouncementIndicatorTrend + 
     vcov(model)["SDILAnnouncement","SDILImplementation"] * 2 * Data$SDILAnnouncement * Data$SDILImplementation + 
     vcov(model)["SDILAnnouncement","ImplementationTrend"] * 2 * Data$SDILAnnouncement * Data$ImplementationTrend + 
     vcov(model)["SDILAnnouncement","ImplementationIndicatorLevel"] * 2 * Data$SDILAnnouncement * Data$ImplementationIndicatorLevel + 
     vcov(model)["SDILAnnouncement","ImplementationIndicatorTrend"] * 2 * Data$SDILAnnouncement * Data$ImplementationIndicatorTrend + 
     vcov(model)["SDILAnnouncement","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$SDILAnnouncement * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["SDILAnnouncement","December"] * 2 * Data$SDILAnnouncement * Data$December + 
     vcov(model)["SDILAnnouncement","January"] * 2 * Data$SDILAnnouncement * Data$January + 
     vcov(model)["SDILAnnouncement","AverageMonthlyTemperature"] * 2 * Data$SDILAnnouncement * Data$AverageMonthlyTemperature + 
     
     vcov(model)["AnnouncementTrend","AnnouncementIndicatorLevel"] * 2 * Data$AnnouncementTrend * Data$AnnouncementIndicatorLevel + 
     vcov(model)["AnnouncementTrend","AnnouncementIndicatorTrend"] * 2 * Data$AnnouncementTrend * Data$AnnouncementIndicatorTrend + 
     vcov(model)["AnnouncementTrend","SDILImplementation"] * 2 * Data$AnnouncementTrend * Data$SDILImplementation + 
     vcov(model)["AnnouncementTrend","ImplementationTrend"] * 2 * Data$AnnouncementTrend * Data$ImplementationTrend + 
     vcov(model)["AnnouncementTrend","ImplementationIndicatorLevel"] * 2 * Data$AnnouncementTrend * Data$ImplementationIndicatorLevel + 
     vcov(model)["AnnouncementTrend","ImplementationIndicatorTrend"] * 2 * Data$AnnouncementTrend * Data$ImplementationIndicatorTrend + 
     vcov(model)["AnnouncementTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementTrend * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["AnnouncementTrend","December"] * 2 * Data$AnnouncementTrend * Data$December + 
     vcov(model)["AnnouncementTrend","January"] * 2 * Data$AnnouncementTrend * Data$January + 
     vcov(model)["AnnouncementTrend","AverageMonthlyTemperature"] * 2 * Data$AnnouncementTrend * Data$AverageMonthlyTemperature + 
     
     vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorTrend"] * 2 * Data$AnnouncementIndicatorLevel * Data$AnnouncementIndicatorTrend + 
     vcov(model)["AnnouncementIndicatorLevel","SDILImplementation"] * 2 * Data$AnnouncementIndicatorLevel * Data$SDILImplementation + 
     vcov(model)["AnnouncementIndicatorLevel","ImplementationTrend"] * 2 * Data$AnnouncementIndicatorLevel * Data$ImplementationTrend + 
     vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorLevel"] * 2 * Data$AnnouncementIndicatorLevel * Data$ImplementationIndicatorLevel + 
     vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorTrend"] * 2 * Data$AnnouncementIndicatorLevel * Data$ImplementationIndicatorTrend + 
     vcov(model)["AnnouncementIndicatorLevel","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementIndicatorLevel * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["AnnouncementIndicatorLevel","December"] * 2 * Data$AnnouncementIndicatorLevel * Data$December + 
     vcov(model)["AnnouncementIndicatorLevel","January"] * 2 * Data$AnnouncementIndicatorLevel * Data$January + 
     vcov(model)["AnnouncementIndicatorLevel","AverageMonthlyTemperature"] * 2 * Data$AnnouncementIndicatorLevel * Data$AverageMonthlyTemperature + 
     
     vcov(model)["AnnouncementIndicatorTrend","SDILImplementation"] * 2 * Data$AnnouncementIndicatorTrend * Data$SDILImplementation + 
     vcov(model)["AnnouncementIndicatorTrend","ImplementationTrend"] * 2 * Data$AnnouncementIndicatorTrend * Data$ImplementationTrend + 
     vcov(model)["AnnouncementIndicatorTrend","ImplementationIndicatorLevel"] * 2 * Data$AnnouncementIndicatorTrend * Data$ImplementationIndicatorLevel + 
     vcov(model)["AnnouncementIndicatorTrend","ImplementationIndicatorTrend"] * 2 * Data$AnnouncementIndicatorTrend * Data$ImplementationIndicatorTrend + 
     vcov(model)["AnnouncementIndicatorTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementIndicatorTrend * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["AnnouncementIndicatorTrend","December"] * 2 * Data$AnnouncementIndicatorTrend * Data$December + 
     vcov(model)["AnnouncementIndicatorTrend","January"] * 2 * Data$AnnouncementIndicatorTrend * Data$January + 
     vcov(model)["AnnouncementIndicatorTrend","AverageMonthlyTemperature"] * 2 * Data$AnnouncementIndicatorTrend * Data$AverageMonthlyTemperature + 
     
     vcov(model)["SDILImplementation","ImplementationTrend"] * 2 * Data$SDILImplementation * Data$ImplementationTrend + 
     vcov(model)["SDILImplementation","ImplementationIndicatorLevel"] * 2 * Data$SDILImplementation * Data$ImplementationIndicatorLevel + 
     vcov(model)["SDILImplementation","ImplementationIndicatorTrend"] * 2 * Data$SDILImplementation * Data$ImplementationIndicatorTrend + 
     vcov(model)["SDILImplementation","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$SDILImplementation * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["SDILImplementation","December"] * 2 * Data$SDILImplementation * Data$December + 
     vcov(model)["SDILImplementation","January"] * 2 * Data$SDILImplementation * Data$January + 
     vcov(model)["SDILImplementation","AverageMonthlyTemperature"] * 2 * Data$SDILImplementation * Data$AverageMonthlyTemperature + 
     
     vcov(model)["ImplementationTrend","ImplementationIndicatorLevel"] * 2 * Data$ImplementationTrend * Data$ImplementationIndicatorLevel + 
     vcov(model)["ImplementationTrend","ImplementationIndicatorTrend"] * 2 * Data$ImplementationTrend * Data$ImplementationIndicatorTrend + 
     vcov(model)["ImplementationTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$ImplementationTrend * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["ImplementationTrend","December"] * 2 * Data$ImplementationTrend * Data$December + 
     vcov(model)["ImplementationTrend","January"] * 2 * Data$ImplementationTrend * Data$January + 
     vcov(model)["ImplementationTrend","AverageMonthlyTemperature"] * 2 * Data$ImplementationTrend * Data$AverageMonthlyTemperature + 
     
     vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorTrend"] * 2 * Data$ImplementationIndicatorLevel * Data$ImplementationIndicatorTrend +
     vcov(model)["ImplementationIndicatorLevel","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$ImplementationIndicatorLevel * Data$AnnouncementIndicatorTrend ^2 +
     vcov(model)["ImplementationIndicatorLevel","December"] * 2 * Data$ImplementationIndicatorLevel * Data$December +
     vcov(model)["ImplementationIndicatorLevel","January"] * 2 * Data$ImplementationIndicatorLevel * Data$January +
     vcov(model)["ImplementationIndicatorLevel","AverageMonthlyTemperature"] * 2 * Data$ImplementationIndicatorLevel * Data$AverageMonthlyTemperature +
     
     vcov(model)["ImplementationIndicatorTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$ImplementationIndicatorTrend * Data$AnnouncementIndicatorTrend ^2 +
     vcov(model)["ImplementationIndicatorTrend","December"] * 2 * Data$ImplementationIndicatorTrend * Data$December +
     vcov(model)["ImplementationIndicatorTrend","January"] * 2 * Data$ImplementationIndicatorTrend * Data$January +
     vcov(model)["ImplementationIndicatorTrend","AverageMonthlyTemperature"] * 2 * Data$ImplementationIndicatorTrend * Data$AverageMonthlyTemperature +
     
     vcov(model)["I(AnnouncementIndicatorTrend^2)","December"] * 2 * Data$AnnouncementIndicatorTrend ^2 * Data$December + 
     vcov(model)["I(AnnouncementIndicatorTrend^2)","January"] * 2 * Data$AnnouncementIndicatorTrend ^2 * Data$December +
     vcov(model)["I(AnnouncementIndicatorTrend^2)","AverageMonthlyTemperature"] * 2 * Data$AnnouncementIndicatorTrend ^2 * Data$AverageMonthlyTemperature +
     
     vcov(model)["December","January"] * 2 * Data$December * Data$January + 
     vcov(model)["December","AverageMonthlyTemperature"] * 2 * Data$December * Data$AverageMonthlyTemperature + 
     
     vcov(model)["January","AverageMonthlyTemperature"] * 2 * Data$January * Data$AverageMonthlyTemperature)[TimePointI1]

###############################################################################
#FIFTEEN TERMS - WITHOUT: QUADRATIC
###############################################################################

VarWithout <- 
  (vcov(model)["(Intercept)","(Intercept)"] + 
     vcov(model)["Time","Time"] * Data$Time ^ 2 +
     vcov(model)["Indicator","Indicator"] * Data$Indicator ^ 2 +
     vcov(model)["IndicatorTime","IndicatorTime"] * Data$IndicatorTime ^ 2 +
     vcov(model)["SDILAnnouncement","SDILAnnouncement"] * Data$SDILAnnouncement ^ 2 +
     vcov(model)["AnnouncementTrend","AnnouncementTrend"] * Data$AnnouncementTrend ^ 2 +
     vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorLevel"] * Data$AnnouncementIndicatorLevel ^ 2 +
     vcov(model)["AnnouncementIndicatorTrend","AnnouncementIndicatorTrend"] * Data$AnnouncementIndicatorTrend ^ 2 +
     vcov(model)["I(AnnouncementIndicatorTrend^2)","I(AnnouncementIndicatorTrend^2)"] * Data$AnnouncementIndicatorTrend ^ 4 +
     vcov(model)["December","December"] * Data$December ^ 2 +
     vcov(model)["January","January"] * Data$January ^ 2 +
     vcov(model)["AverageMonthlyTemperature","AverageMonthlyTemperature"] * Data$AverageMonthlyTemperature ^ 2 +
     
     #COVARIANCE
     vcov(model)["(Intercept)","Time"] * 2 * Data$Time +
     vcov(model)["(Intercept)","Indicator"] * 2 * Data$Indicator +
     vcov(model)["(Intercept)","IndicatorTime"] * 2 * Data$IndicatorTime +
     vcov(model)["(Intercept)","SDILAnnouncement"] * 2 * Data$SDILAnnouncement +
     vcov(model)["(Intercept)","AnnouncementTrend"] * 2 * Data$AnnouncementTrend +
     vcov(model)["(Intercept)","AnnouncementIndicatorLevel"] * 2 * Data$AnnouncementIndicatorLevel +
     vcov(model)["(Intercept)","AnnouncementIndicatorTrend"] * 2 * Data$AnnouncementIndicatorTrend +
     vcov(model)["(Intercept)","SDILImplementation"] * 2 * Data$SDILImplementation +
     vcov(model)["(Intercept)","ImplementationTrend"] * 2 * Data$ImplementationTrend +
     vcov(model)["(Intercept)","ImplementationIndicatorLevel"] * 2 * Data$ImplementationIndicatorLevel +
     vcov(model)["(Intercept)","ImplementationIndicatorTrend"] * 2 * Data$ImplementationIndicatorTrend +
     vcov(model)["(Intercept)","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementIndicatorTrend^2 +
     vcov(model)["(Intercept)","December"] * 2 * Data$December +
     vcov(model)["(Intercept)","January"] * 2 * Data$January +
     vcov(model)["(Intercept)","AverageMonthlyTemperature"] * 2 * Data$AverageMonthlyTemperature +
     
     vcov(model)["Time","Indicator"] * 2 * Data$Time * Data$Indicator +
     vcov(model)["Time","IndicatorTime"] * 2 * Data$Time * Data$IndicatorTime +
     vcov(model)["Time","SDILAnnouncement"] * 2 * Data$Time * Data$SDILAnnouncement +
     vcov(model)["Time","AnnouncementTrend"] * 2 * Data$Time * Data$AnnouncementTrend +
     vcov(model)["Time","AnnouncementIndicatorLevel"] * 2 * Data$Time * Data$AnnouncementIndicatorLevel +
     vcov(model)["Time","AnnouncementIndicatorTrend"] * 2 * Data$Time * Data$AnnouncementIndicatorTrend +
     vcov(model)["Time","SDILImplementation"] * 2 * Data$Time * Data$SDILImplementation +
     vcov(model)["Time","ImplementationTrend"] * 2 * Data$Time * Data$ImplementationTrend +
     vcov(model)["Time","ImplementationIndicatorLevel"] * 2 * Data$Time * Data$ImplementationIndicatorLevel +
     vcov(model)["Time","ImplementationIndicatorTrend"] * 2 * Data$Time * Data$ImplementationIndicatorTrend +
     vcov(model)["Time","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$Time * Data$AnnouncementIndicatorTrend^2 +
     vcov(model)["Time","December"] * 2 * Data$Time * Data$December +
     vcov(model)["Time","January"] * 2 * Data$Time * Data$January +
     vcov(model)["Time","AverageMonthlyTemperature"] * 2 * Data$Time * Data$AverageMonthlyTemperature +
     
     vcov(model)["Indicator","IndicatorTime"] * 2 * Data$Indicator * Data$IndicatorTime + 
     vcov(model)["Indicator","SDILAnnouncement"] * 2 * Data$Indicator * Data$SDILAnnouncement +
     vcov(model)["Indicator","AnnouncementTrend"] * 2 * Data$Indicator * Data$AnnouncementTrend +
     vcov(model)["Indicator","AnnouncementIndicatorLevel"] * 2 * Data$Indicator * Data$AnnouncementIndicatorLevel +
     vcov(model)["Indicator","AnnouncementIndicatorTrend"] * 2 * Data$Indicator * Data$AnnouncementIndicatorTrend +
     vcov(model)["Indicator","SDILImplementation"] * 2 * Data$Indicator * Data$SDILImplementation +
     vcov(model)["Indicator","ImplementationTrend"] * 2 * Data$Indicator * Data$ImplementationTrend +
     vcov(model)["Indicator","ImplementationIndicatorLevel"] * 2 * Data$Indicator * Data$ImplementationIndicatorLevel +
     vcov(model)["Indicator","ImplementationIndicatorTrend"] * 2 * Data$Indicator * Data$ImplementationIndicatorTrend +
     vcov(model)["Indicator","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$Indicator * Data$AnnouncementIndicatorTrend^2 +
     vcov(model)["Indicator","December"] * 2 * Data$Indicator * Data$December +
     vcov(model)["Indicator","January"] * 2 * Data$Indicator * Data$January +
     vcov(model)["Indicator","AverageMonthlyTemperature"] * 2 * Data$Indicator * Data$AverageMonthlyTemperature +
     
     vcov(model)["IndicatorTime","SDILAnnouncement"] * 2 * Data$IndicatorTime * Data$SDILAnnouncement +
     vcov(model)["IndicatorTime","AnnouncementTrend"] * 2 * Data$IndicatorTime * Data$AnnouncementTrend + 
     vcov(model)["IndicatorTime","AnnouncementIndicatorLevel"] * 2 * Data$IndicatorTime * Data$AnnouncementIndicatorLevel + 
     vcov(model)["IndicatorTime","AnnouncementIndicatorTrend"] * 2 * Data$IndicatorTime * Data$AnnouncementIndicatorTrend + 
     vcov(model)["IndicatorTime","SDILImplementation"] * 2 * Data$IndicatorTime * Data$SDILImplementation +
     vcov(model)["IndicatorTime","ImplementationTrend"] * 2 * Data$IndicatorTime * Data$ImplementationTrend + 
     vcov(model)["IndicatorTime","ImplementationIndicatorLevel"] * 2 * Data$IndicatorTime * Data$ImplementationIndicatorLevel + 
     vcov(model)["IndicatorTime","ImplementationIndicatorTrend"] * 2 * Data$IndicatorTime * Data$ImplementationIndicatorTrend + 
     vcov(model)["IndicatorTime","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$IndicatorTime * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["IndicatorTime","December"] * 2 * Data$IndicatorTime * Data$December + 
     vcov(model)["IndicatorTime","January"] * 2 * Data$IndicatorTime * Data$January + 
     vcov(model)["IndicatorTime","AverageMonthlyTemperature"] * 2 * Data$IndicatorTime * Data$AverageMonthlyTemperature + 
     
     vcov(model)["SDILAnnouncement","AnnouncementTrend"] * 2 * Data$SDILAnnouncement * Data$AnnouncementTrend + 
     vcov(model)["SDILAnnouncement","AnnouncementIndicatorLevel"] * 2 * Data$SDILAnnouncement * Data$AnnouncementIndicatorLevel + 
     vcov(model)["SDILAnnouncement","AnnouncementIndicatorTrend"] * 2 * Data$SDILAnnouncement * Data$AnnouncementIndicatorTrend + 
     vcov(model)["SDILAnnouncement","SDILImplementation"] * 2 * Data$SDILAnnouncement * Data$SDILImplementation + 
     vcov(model)["SDILAnnouncement","ImplementationTrend"] * 2 * Data$SDILAnnouncement * Data$ImplementationTrend + 
     vcov(model)["SDILAnnouncement","ImplementationIndicatorLevel"] * 2 * Data$SDILAnnouncement * Data$ImplementationIndicatorLevel + 
     vcov(model)["SDILAnnouncement","ImplementationIndicatorTrend"] * 2 * Data$SDILAnnouncement * Data$ImplementationIndicatorTrend + 
     vcov(model)["SDILAnnouncement","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$SDILAnnouncement * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["SDILAnnouncement","December"] * 2 * Data$SDILAnnouncement * Data$December + 
     vcov(model)["SDILAnnouncement","January"] * 2 * Data$SDILAnnouncement * Data$January + 
     vcov(model)["SDILAnnouncement","AverageMonthlyTemperature"] * 2 * Data$SDILAnnouncement * Data$AverageMonthlyTemperature + 
     
     vcov(model)["AnnouncementTrend","AnnouncementIndicatorLevel"] * 2 * Data$AnnouncementTrend * Data$AnnouncementIndicatorLevel + 
     vcov(model)["AnnouncementTrend","AnnouncementIndicatorTrend"] * 2 * Data$AnnouncementTrend * Data$AnnouncementIndicatorTrend + 
     vcov(model)["AnnouncementTrend","SDILImplementation"] * 2 * Data$AnnouncementTrend * Data$SDILImplementation + 
     vcov(model)["AnnouncementTrend","ImplementationTrend"] * 2 * Data$AnnouncementTrend * Data$ImplementationTrend + 
     vcov(model)["AnnouncementTrend","ImplementationIndicatorLevel"] * 2 * Data$AnnouncementTrend * Data$ImplementationIndicatorLevel + 
     vcov(model)["AnnouncementTrend","ImplementationIndicatorTrend"] * 2 * Data$AnnouncementTrend * Data$ImplementationIndicatorTrend + 
     vcov(model)["AnnouncementTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementTrend * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["AnnouncementTrend","December"] * 2 * Data$AnnouncementTrend * Data$December + 
     vcov(model)["AnnouncementTrend","January"] * 2 * Data$AnnouncementTrend * Data$January + 
     vcov(model)["AnnouncementTrend","AverageMonthlyTemperature"] * 2 * Data$AnnouncementTrend * Data$AverageMonthlyTemperature + 
     
     vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorTrend"] * 2 * Data$AnnouncementIndicatorLevel * Data$AnnouncementIndicatorTrend + 
     vcov(model)["AnnouncementIndicatorLevel","SDILImplementation"] * 2 * Data$AnnouncementIndicatorLevel * Data$SDILImplementation + 
     vcov(model)["AnnouncementIndicatorLevel","ImplementationTrend"] * 2 * Data$AnnouncementIndicatorLevel * Data$ImplementationTrend + 
     vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorLevel"] * 2 * Data$AnnouncementIndicatorLevel * Data$ImplementationIndicatorLevel + 
     vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorTrend"] * 2 * Data$AnnouncementIndicatorLevel * Data$ImplementationIndicatorTrend + 
     vcov(model)["AnnouncementIndicatorLevel","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementIndicatorLevel * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["AnnouncementIndicatorLevel","December"] * 2 * Data$AnnouncementIndicatorLevel * Data$December + 
     vcov(model)["AnnouncementIndicatorLevel","January"] * 2 * Data$AnnouncementIndicatorLevel * Data$January + 
     vcov(model)["AnnouncementIndicatorLevel","AverageMonthlyTemperature"] * 2 * Data$AnnouncementIndicatorLevel * Data$AverageMonthlyTemperature + 
     
     vcov(model)["AnnouncementIndicatorTrend","SDILImplementation"] * 2 * Data$AnnouncementIndicatorTrend * Data$SDILImplementation + 
     vcov(model)["AnnouncementIndicatorTrend","ImplementationTrend"] * 2 * Data$AnnouncementIndicatorTrend * Data$ImplementationTrend + 
     vcov(model)["AnnouncementIndicatorTrend","ImplementationIndicatorLevel"] * 2 * Data$AnnouncementIndicatorTrend * Data$ImplementationIndicatorLevel + 
     vcov(model)["AnnouncementIndicatorTrend","ImplementationIndicatorTrend"] * 2 * Data$AnnouncementIndicatorTrend * Data$ImplementationIndicatorTrend + 
     vcov(model)["AnnouncementIndicatorTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$AnnouncementIndicatorTrend * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["AnnouncementIndicatorTrend","December"] * 2 * Data$AnnouncementIndicatorTrend * Data$December + 
     vcov(model)["AnnouncementIndicatorTrend","January"] * 2 * Data$AnnouncementIndicatorTrend * Data$January + 
     vcov(model)["AnnouncementIndicatorTrend","AverageMonthlyTemperature"] * 2 * Data$AnnouncementIndicatorTrend * Data$AverageMonthlyTemperature + 
     
     vcov(model)["SDILImplementation","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$SDILImplementation * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["SDILImplementation","December"] * 2 * Data$SDILImplementation * Data$December + 
     vcov(model)["SDILImplementation","January"] * 2 * Data$SDILImplementation * Data$January + 
     vcov(model)["SDILImplementation","AverageMonthlyTemperature"] * 2 * Data$SDILImplementation * Data$AverageMonthlyTemperature + 
     
     vcov(model)["ImplementationTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$ImplementationTrend * Data$AnnouncementIndicatorTrend^2 + 
     vcov(model)["ImplementationTrend","December"] * 2 * Data$ImplementationTrend * Data$December + 
     vcov(model)["ImplementationTrend","January"] * 2 * Data$ImplementationTrend * Data$January + 
     vcov(model)["ImplementationTrend","AverageMonthlyTemperature"] * 2 * Data$ImplementationTrend * Data$AverageMonthlyTemperature + 
     
     vcov(model)["ImplementationIndicatorLevel","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$ImplementationIndicatorLevel * Data$AnnouncementIndicatorTrend ^2 +
     vcov(model)["ImplementationIndicatorLevel","December"] * 2 * Data$ImplementationIndicatorLevel * Data$December +
     vcov(model)["ImplementationIndicatorLevel","January"] * 2 * Data$ImplementationIndicatorLevel * Data$January +
     vcov(model)["ImplementationIndicatorLevel","AverageMonthlyTemperature"] * 2 * Data$ImplementationIndicatorLevel * Data$AverageMonthlyTemperature +
     
     vcov(model)["ImplementationIndicatorTrend","I(AnnouncementIndicatorTrend^2)"] * 2 * Data$ImplementationIndicatorTrend * Data$AnnouncementIndicatorTrend ^2 +
     vcov(model)["ImplementationIndicatorTrend","December"] * 2 * Data$ImplementationIndicatorTrend * Data$December +
     vcov(model)["ImplementationIndicatorTrend","January"] * 2 * Data$ImplementationIndicatorTrend * Data$January +
     vcov(model)["ImplementationIndicatorTrend","AverageMonthlyTemperature"] * 2 * Data$ImplementationIndicatorTrend * Data$AverageMonthlyTemperature +
     
     vcov(model)["I(AnnouncementIndicatorTrend^2)","December"] * 2 * Data$AnnouncementIndicatorTrend ^2 * Data$December + 
     vcov(model)["I(AnnouncementIndicatorTrend^2)","January"] * 2 * Data$AnnouncementIndicatorTrend ^2 * Data$December +
     vcov(model)["I(AnnouncementIndicatorTrend^2)","AverageMonthlyTemperature"] * 2 * Data$AnnouncementIndicatorTrend ^2 * Data$AverageMonthlyTemperature +
     
     vcov(model)["December","January"] * 2 * Data$December * Data$January + 
     vcov(model)["December","AverageMonthlyTemperature"] * 2 * Data$December * Data$AverageMonthlyTemperature + 
     
     vcov(model)["January","AverageMonthlyTemperature"] * 2 * Data$January * Data$AverageMonthlyTemperature)[TimePointI2]

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - LOWER CI
###############################################################################

LowerCIImplementation <- (AbsDiffImplementation - 1.96 * sqrt(VarWith - VarWithout))

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - UPPER CI
###############################################################################

UpperCIImplementation <- (AbsDiffImplementation + 1.96 * sqrt(VarWith - VarWithout))

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - COMBINE RESULTS
###############################################################################

list("Implementation.AbsDiff"          = AbsDiffImplementation*(1000/n),
     "Implementation.AbsDiff.LowerCI"  = LowerCIImplementation*(1000/n),
     "Implementation.AbsDiff.UpperCI"  = UpperCIImplementation*(1000/n))
}
