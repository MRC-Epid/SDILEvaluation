###############################################################################
#RELATIVE DIFFERENCES FUNCTIONS FOR THE IMPLEMENTATION
#DAVID PELL
#21/11/2019
###############################################################################
#CREATE THREE FUNCTIONS
#ONE THAT INCLUDES NEITHER QUADRATIC NOR EASTER TERMS
#ONE THAT INCLUDES QUADRATIC TERMS
#ONE THAT INCLUDES EASTER AND POST EASTER TERMS
###############################################################################

RelativeDifferencesI.fun <- function(model, n=1, TimePointI1=264, TimePointI2=264){
  
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
  
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES - NO QUADRATIC TERMS NO EASTER TERM
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

covariates <- c("(Intercept)",                   #1
                "Indicator",                     #2    
                "Time",                          #3
                "IndicatorTime",                 #4
                "SDILAnnouncement",              #5
                "AnnouncementIndicatorLevel",    #6
                "AnnouncementTrend",             #7
                "AnnouncementIndicatorTrend",    #8
                "AverageMonthlyTemperature",     #9
                "December",                      #10
                "January",                       #11
                "SDILImplementation",            #12
                "ImplementationIndicatorLevel",  #13
                "ImplementationTrend",           #14
                "ImplementationIndicatorTrend")  #15
                
###############################################################################
#EXTRACT MODEL COEFFICIENTS
###############################################################################
  
estmean <- coef(model)[covariates]

###############################################################################
#DROP COLUMNS FROM VARIANCE COVARIANCE MATRIX
###############################################################################

estvar <- vcov(model)[,covariates]

###############################################################################
#DROP ROWS FROM VARIANCE COVARIANCE MATRIX
###############################################################################

estvar <- estvar[covariates,]

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
  coef(model)[[covariates[15]]] * eval(parse(text=paste0("Data$",covariates[15])))[TimePointI1]

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
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")")))
#   coef(model)[[covariates[12]]]  * eval(parse(text=paste0("Data$",covariates[12])))[TimePointI2] +
#   coef(model)[[covariates[13]]]  * eval(parse(text=paste0("Data$",covariates[13])))[TimePointI2] +
#   coef(model)[[covariates[14]]]  * eval(parse(text=paste0("Data$",covariates[14])))[TimePointI2] +
#   coef(model)[[covariates[15]]]  * eval(parse(text=paste0("Data$",covariates[15])))[TimePointI2]

###############################################################################
#GET RELATIVE DIFFERENCE
###############################################################################

est <- (YWith - YWithout) / YWithout

###############################################################################
#BUILD MATRIX FOR MDM FUNCTION
###############################################################################

###############################################################################
#WITH HAS ALL THE TERMS
###############################################################################

With <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x12*%f+x13*%f+x14*%f+x15*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointI1],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
          eval(parse(text=paste0("Data$",covariates[12])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[13])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[14])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[15])))[TimePointI1])

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS - COUNTERFACTUAL
###############################################################################

Without <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f",
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointI2],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))))
#           eval(parse(text=paste0("Data$",covariates[12])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[13])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[14])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[15])))[TimePointI2]

###############################################################################
#(YWITH - YWITHOUT) / YWITHOUT
###############################################################################

g <- as.formula(paste0("~((",With,") - (",Without,")) / (",Without,")"))

###############################################################################
#CALCULATE STANDARD ERROR USING THE DELTA METHOD
###############################################################################

se <- msm::deltamethod(g, mean=estmean,cov=estvar)
  
###############################################################################
#COMBINE ESTIMATES
###############################################################################

list("Implementation.pcDiff"=est*100, 
     "Implementation.pcDiff.LowerCI"=(est + (-1.96 * se)) * 100,
     "Implementation.pcDiff.UpperCI"=(est + ( 1.96 * se)) * 100)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES + ANNOUNCEMENT QUAD TERM, NO EASTER
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

RelativeDifferencesQuadI.fun <- function(model,n=1, TimePointI1=264, TimePointI2=264){
  
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
  
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES - QUAD, NO EASTER
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

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
#EXTRACT MODEL COEFFICIENTS
###############################################################################

estmean <- coef(model)[covariates]

###############################################################################
#DROP COLUMNS FROM VARIANCE COVARIANCE MATRIX
###############################################################################

estvar <- vcov(model)[,covariates]

###############################################################################
#DROP ROWS FROM VARIANCE COVARIANCE MATRIX
###############################################################################

estvar <- estvar[covariates,]

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
#GET RELATIVE DIFFERENCE
###############################################################################

est <- (YWith - YWithout) / YWithout

###############################################################################
#BUILD MATRIX FOR MDM FUNCTION
###############################################################################

###############################################################################
#WITH HAS ALL THE TERMS
###############################################################################

With <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x12*%f+x13*%f+x14*%f+x15*%f+x16*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointI1],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
          eval(parse(text=paste0("Data$",covariates[12])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[13])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[14])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[15])))[TimePointI1],
          Data$AnnouncementIndicatorTrend[TimePointI1]^2)

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS - COUNTERFACTUAL
###############################################################################

Without <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x16*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointI2],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
#           eval(parse(text=paste0("Data$",covariates[12])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[13])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[14])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[15])))[TimePointI2],
          Data$AnnouncementIndicatorTrend[TimePointI2]^2)

###############################################################################
#(YWITH - YWITHOUT) / YWITHOUT
###############################################################################

g <- as.formula(paste0("~((",With,") - (",Without,")) / (",Without,")"))

################################################################################
#CALCULATE STANDARD ERROR USING THE DELTA METHOD
###############################################################################
  
se <- msm::deltamethod(g, mean=estmean,cov=estvar)
  
###############################################################################
#COMBINE ESTIMATES
###############################################################################

list("Implementation.pcDiff"=est*100, 
     "Implementation.pcDiff.LowerCI"=(est + (-1.96 * se)) * 100,
     "Implementation.pcDiff.UpperCI"=(est + ( 1.96 * se)) * 100)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES TERM - EASTER AND POST EASTER TERMS ONLY
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

RelativeDifferencesEasterI.fun <- function(model,n=1, TimePointI1=264, TimePointI2=264){
  
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
  
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES - NO QUADRATIC TERMS, INCLUDES EASTER
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

covariates <- c("(Intercept)",                      #1
                "Indicator",                        #2    
                "Time",                             #3
                "IndicatorTime",                    #4
                "SDILAnnouncement",                 #5
                "AnnouncementIndicatorLevel",       #6
                "AnnouncementTrend",                #7
                "AnnouncementIndicatorTrend",       #8
                "AverageMonthlyTemperature",        #9
                "December",                         #10
                "January",                          #11
                "SDILImplementation",               #12
                "ImplementationIndicatorLevel",     #13
                "ImplementationTrend",              #14
                "ImplementationIndicatorTrend",     #15
                "Easter",                           #16
                "PostEaster")                       #17

###############################################################################
#EXTRACT MODEL COEFFICIENTS
###############################################################################

estmean <- coef(model)[covariates]

###############################################################################
#DROP COLUMNS FROM VARIANCE COVARIANCE MATRIX
###############################################################################

estvar <- vcov(model)[,covariates]

###############################################################################
#DROP ROWS FROM VARIANCE COVARIANCE MATRIX
###############################################################################

estvar <- estvar[covariates,]

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
  coef(model)[[covariates[16]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[16],")"))) + 
  coef(model)[[covariates[17]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[17],")")))

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
#   coef(model)[[covariates[12]]] * eval(parse(text=paste0("Data$",covariates[12])))[TimePointI2] +
#   coef(model)[[covariates[13]]] * eval(parse(text=paste0("Data$",covariates[13])))[TimePointI2] +
#   coef(model)[[covariates[14]]] * eval(parse(text=paste0("Data$",covariates[14])))[TimePointI2] +                          
#   coef(model)[[covariates[15]]] * eval(parse(text=paste0("Data$",covariates[15])))[TimePointI2] +
  coef(model)[[covariates[16]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[16],")"))) + 
  coef(model)[[covariates[17]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[17],")"))) 
  
###############################################################################
#GET RELATIVE DIFFERENCE
###############################################################################

est <- (YWith - YWithout) / YWithout

###############################################################################
#BUILD MATRIX FOR MDM FUNCTION
###############################################################################

###############################################################################
#WITH HAS ALL THE TERMS
###############################################################################

With <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x12*%f+x13*%f+x14*%f+x15*%f+x16*%f+x17*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointI1],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
          eval(parse(text=paste0("Data$",covariates[12])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[13])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[14])))[TimePointI1],
          eval(parse(text=paste0("Data$",covariates[15])))[TimePointI1],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[16],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[17],")"))))

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS - COUNTERFACTUAL
###############################################################################

Without <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x16*%f+x17*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointI2],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointI2],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
#           eval(parse(text=paste0("Data$",covariates[12])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[13])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[14])))[TimePointI2],
#           eval(parse(text=paste0("Data$",covariates[15])))[TimePointI2],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[16],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[17],")"))))

###############################################################################
#(YWITH - YWITHOUT) / YWITHOUT
###############################################################################

g <- as.formula(paste0("~((",With,") - (",Without,")) / (",Without,")"))
  
################################################################################
#CALCULATE STANDARD ERROR USING THE DELTA METHOD
###############################################################################
  
se <- msm::deltamethod(g, mean=estmean,cov=estvar)
  
###############################################################################
#COMBINE ESTIMATES
###############################################################################
  
list("Implementation.pcDiff"=est*100,
     "Implementation.pcDiff.LowerCI"=(est + (-1.96 * se)) * 100,
     "Implementation.pcDiff.UpperCI"=(est + ( 1.96 * se)) * 100)
}

###############################################################################
#RELATIVE DIFFERENCE FUNCTION SELECTION
###############################################################################

RelativeChangeImplementation.fun <-function(model,n=1,TimePointI1=264,TimePointI2=264){
  model.object = eval(parse(text=paste0(model)))
  ifelse(sum(names(coef(model.object)) %in% "I(AnnouncementIndicatorTrend^2)")==1,
         list(RelativeDifferencesQuadI.fun(model,n,TimePointI1,TimePointI2)),
         ifelse(sum(names(coef(model.object)) %in% "Easter")==1,
                list(RelativeDifferencesEasterI.fun(model,n,TimePointI1,TimePointI2)),
                list(RelativeDifferencesI.fun(model,n,TimePointI1,TimePointI2))))
}
