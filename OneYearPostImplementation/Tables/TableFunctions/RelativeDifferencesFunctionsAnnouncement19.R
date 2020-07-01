###############################################################################
#RELATIVE DIFFERENCES FUNCTIONS FOR THE ANNOUNCEMENT
#DAVID PELL
#20/11/2019
###############################################################################
#CREATE THREE FUNCTIONS
#ONE THAT INCLUDES NEITHER QUADRATIC NOR EASTER TERMS
#ONE THAT INCLUDES QUADRATIC TERMS
#ONE THAT INCLUDES EASTER AND POST EASTER
###############################################################################

RelativeDifferencesA.fun <- function(model,n=1, TimePointA=213){
  
###############################################################################
#COPY MODEL NAME
###############################################################################
  
modelName <- model  
  
###############################################################################
#STRIP "SUGAR" FROM MODEL NAME
###############################################################################
  
model <- ifelse(model == "LowSugar" |
                model =="NoSugar",model, sub("Sugar", "", model))  

###############################################################################
#STRIP SENSITIVITY ANALYSES INDICATORS FROM MODEL NAME
###############################################################################

model <- sub("05M", "", model)
model <- sub("1M", "", model)

###############################################################################
#SELECT DATA 
#FIRST CHECKING WHETHER THE DATA SET CONTAINS SDIL2 THEN IF NOT SDILTOTAL
###############################################################################
  
if("SDIL2" %in% colnames(WeeklyVolume))
{
  Data <-  WeeklyVolume[WeeklyVolume$SDIL2 %in% c(model,"Toiletries")]
} else {
  if("SDILTotal" %in% colnames(WeeklyVolume))
  {
    Data <-  WeeklyVolume[WeeklyVolume$SDILTotal %in% c(model,"Toiletries")]
  }
}
  
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
#RELATIVE CHANGES - NO QUADRATIC TERMS NO EASTER TERMS
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
                "January")                       #11

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
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointA] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointA] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointA] +
  coef(model)[[covariates[5]]]  * eval(parse(text=paste0("Data$",covariates[5])))[TimePointA] +
  coef(model)[[covariates[6]]]  * eval(parse(text=paste0("Data$",covariates[6])))[TimePointA] +  
  coef(model)[[covariates[7]]]  * eval(parse(text=paste0("Data$",covariates[7])))[TimePointA] +
  coef(model)[[covariates[8]]]  * eval(parse(text=paste0("Data$",covariates[8])))[TimePointA] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")")))

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS
###############################################################################

YWithout =  
  coef(model)[[covariates[1]]]  + 
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointA] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointA] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointA] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")")))

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
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointA],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))))

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS
###############################################################################

Without <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x9*%f+x10*%f+x11*%f",
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointA],
#           eval(parse(text=paste0("Data$",covariates[5])))[TimePointA],
#           eval(parse(text=paste0("Data$",covariates[6])))[TimePointA],
#           eval(parse(text=paste0("Data$",covariates[7])))[TimePointA],
#           eval(parse(text=paste0("Data$",covariates[8])))[TimePointA],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))))

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

list("Announcement.pcDiff"=est*100, 
     "Announcement.pcDiff.LowerCI"=(est + (-1.96 * se)) * 100,
     "Announcement.pcDiff.UpperCI"=(est + ( 1.96 * se)) * 100)
}

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES + QUAD TERM, NO EASTER
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

RelativeDifferencesQuadA.fun <- function(model,n=1, TimePointA=213){
  
###############################################################################
#COPY MODEL NAME
###############################################################################
  
modelName <- model  

###############################################################################
#STRIP "SUGAR" FROM MODEL NAME
###############################################################################

model <- ifelse(model == "LowSugar" |
                model =="NoSugar",model, sub("Sugar", "", model))  

###############################################################################
#STRIP SENSITIVITY ANALYSES INDICATORS FROM MODEL NAME
###############################################################################

model <- sub("05M", "", model)
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

covariates <- c("(Intercept)",                     #1
                "Indicator",                       #2    
                "Time",                            #3
                "IndicatorTime",                   #4
                "SDILAnnouncement",                #5
                "AnnouncementIndicatorLevel",      #6
                "AnnouncementTrend",               #7
                "AnnouncementIndicatorTrend",      #8
                "AverageMonthlyTemperature",       #9
                "December",                        #10
                "January",                         #11
                "I(AnnouncementIndicatorTrend^2)") #12
                 
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
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointA] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointA] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointA] +
  coef(model)[[covariates[5]]]  * eval(parse(text=paste0("Data$",covariates[5])))[TimePointA] +
  coef(model)[[covariates[6]]]  * eval(parse(text=paste0("Data$",covariates[6])))[TimePointA] +  
  coef(model)[[covariates[7]]]  * eval(parse(text=paste0("Data$",covariates[7])))[TimePointA] +
  coef(model)[[covariates[8]]]  * eval(parse(text=paste0("Data$",covariates[8])))[TimePointA] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))) +
  coef(model)[[covariates[12]]] * Data$AnnouncementIndicatorTrend[TimePointA]^2

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS
###############################################################################

YWithout =  
  coef(model)[[covariates[1]]]  + 
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointA] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointA] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointA] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")")))

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
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x12*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointA],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
          Data$AnnouncementIndicatorTrend[TimePointA]^2)

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS
###############################################################################

Without <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x9*%f+x10*%f+x11*%f",
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[5])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[6])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[7])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[8])))[TimePointA],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))))
          #Data$AnnouncementIndicatorTrend[TimePointA]^2)

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

list("Announcement.pcDiff"=est*100, 
     "Announcement.pcDiff.LowerCI"=(est + (-1.96 * se)) * 100,
     "Announcement.pcDiff.UpperCI"=(est + ( 1.96 * se)) * 100)
}


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
#RELATIVE CHANGES TERM - EASTER AND POST EASTER ONLY
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

RelativeDifferencesEasterA.fun <- function(model,n=1, TimePointA=213){
  
###############################################################################
#COPY MODEL NAME
###############################################################################
  
modelName <- model 
  
###############################################################################
#STRIP "SUGAR" FROM MODEL NAME
###############################################################################
  
model <- ifelse(model == "LowSugar" |
                  model =="NoSugar",model, sub("Sugar", "", model))  

###############################################################################
#STRIP SENSITIVITY ANALYSES INDICATORS FROM MODEL NAME
###############################################################################

model <- sub("05M", "", model)
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

covariates <- c("(Intercept)",                     #1
                "Indicator",                       #2    
                "Time",                            #3
                "IndicatorTime",                   #4
                "SDILAnnouncement",                #5
                "AnnouncementIndicatorLevel",      #6
                "AnnouncementTrend",               #7
                "AnnouncementIndicatorTrend",      #8
                "AverageMonthlyTemperature",       #9
                "December",                        #10
                "January",                         #11
                "Easter",                          #12
                "PostEaster")                      #13

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
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointA] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointA] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointA] +
  coef(model)[[covariates[5]]]  * eval(parse(text=paste0("Data$",covariates[5])))[TimePointA] +
  coef(model)[[covariates[6]]]  * eval(parse(text=paste0("Data$",covariates[6])))[TimePointA] +  
  coef(model)[[covariates[7]]]  * eval(parse(text=paste0("Data$",covariates[7])))[TimePointA] +
  coef(model)[[covariates[8]]]  * eval(parse(text=paste0("Data$",covariates[8])))[TimePointA] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))) + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))) +
  coef(model)[[covariates[12]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[12],")"))) + 
  coef(model)[[covariates[13]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[13],")")))

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS
###############################################################################

YWithout =  
  coef(model)[[covariates[1]]]  + 
  coef(model)[[covariates[2]]]  * eval(parse(text=paste0("Data$",covariates[2])))[TimePointA] + 
  coef(model)[[covariates[3]]]  * eval(parse(text=paste0("Data$",covariates[3])))[TimePointA] +
  coef(model)[[covariates[4]]]  * eval(parse(text=paste0("Data$",covariates[4])))[TimePointA] +
  coef(model)[[covariates[9]]]  * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")")))  + 
  coef(model)[[covariates[10]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))) +
  coef(model)[[covariates[11]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))) +
  coef(model)[[covariates[12]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[12],")"))) +
  coef(model)[[covariates[13]]] * eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[13],")")))

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
  sprintf("x1+x2*%f+x3*%f+x4*%f+x5*%f+x6*%f+x7*%f+x8*%f+x9*%f+x10*%f+x11*%f+x12*%f+x13*%f", 
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[5])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[6])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[7])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[8])))[TimePointA],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[12],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[13],")"))))

###############################################################################
#WITHOUT DROPS THE EXTRA TERMS
###############################################################################

Without <- 
  sprintf("x1+x2*%f+x3*%f+x4*%f+x9*%f+x10*%f+x11*%f+x12*%f+x13*%f",
          eval(parse(text=paste0("Data$",covariates[2])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[3])))[TimePointA],
          eval(parse(text=paste0("Data$",covariates[4])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[5])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[6])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[7])))[TimePointA],
          #           eval(parse(text=paste0("Data$",covariates[8])))[TimePointA],
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[9],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[10],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[11],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[12],")"))),
          eval(parse(text=paste0("mean(Data[Data$SDIL2 != 'Toiletries']$",covariates[13],")"))))

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
  
list("Announcement.pcDiff"=est*100,
     "Announcement.pcDiff.LowerCI"=(est + (-1.96 * se)) * 100,
     "Announcement.pcDiff.UpperCI"=(est + ( 1.96 * se)) * 100)
}

###############################################################################
#RELATIVE DIFFERENCE FUNCTION SELECTION
###############################################################################

RelativeChangeAnnouncement.fun <-function(model,n=1){
  model.object = eval(parse(text=paste0(model)))
  ifelse(sum(names(coef(model.object)) %in% "I(AnnouncementIndicatorTrend^2)")==1,
         list(RelativeDifferencesQuadA.fun(model,n)),
         ifelse(sum(names(coef(model.object)) %in% "Easter")==1,
                list(RelativeDifferencesEasterA.fun(model,n)),
                list(RelativeDifferencesA.fun(model,n))))
}
