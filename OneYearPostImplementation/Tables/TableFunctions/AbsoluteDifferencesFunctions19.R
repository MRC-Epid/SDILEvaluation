###############################################################################
#ABSOLUTE DIFFERENCES FUNCTIONS
#DAVID PELL
#19/11/2019
###############################################################################
#FUNCTION FOR ABSOLUTE DIFFERENCES 
#ONE FUNCTION.DIFFERENT FROM PREVIOUS VERSION AS IT CHECKS FOR A QUADRATIC TERM
#FIRST FUNCTION FOR PRE-IMPLEMENTATION
###############################################################################

AbsoluteChangeAnnouncement.fun <-function(model, n=1, TimePointA=213){

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

model <- sub("05M", "", model)
model <- sub("1M" , "", model)

###############################################################################
#SELECT DATA
###############################################################################
  
Data <- WeeklyVolume[WeeklyVolume$SDIL2 %in% c(model,"Toiletries")]
  
###############################################################################
#ORDER DATA
###############################################################################
  
Data <- Data[order(Data$SDIL2 %in% "Toiletries", decreasing=FALSE)]

################################################################################
#SCALE DATA 
#THIS IS USED TO REDUCE THE UNITS TO KG 
###############################################################################

Data$sugar <- ifelse(Data$SDIL2 != "Toiletries",(Data$sugar/n),Data$sugar)

###############################################################################
#MODEL TO OBJECT
###############################################################################
  
model <- eval(parse(text=paste0(modelName)))

###############################################################################
#QUADRATIC COEFFICIENT (QC) FUNCTION
###############################################################################

QC.fun <- function(coefficient){
  ifelse(sum(names(coef(model)) %in% coefficient)==0,0,coefficients(model)[[coefficient]])
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
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION
#SUMMING THESE TERMS IS THE SAME AS SUBTRACTING THESE TERMS FROM ALL THE TERMS
#THAT SEEMS SELF EVIDENT HOWEVER THE VARIANCE DOES NOT WORK THAT WAY AS NOT ALL 
#TERMS ARE INCLUDED 
#SEE HERE: "/dph/sdil/KWP221/Analysis/RegressionTable/ManualAbsoluteDifferencesPointEstimateCalculations19.R"
###############################################################################

AbsDiffAnnouncement <- 
  coef(model)[["SDILAnnouncement"]]            * Data$SDILAnnouncement[TimePointA] + 
  coef(model)[["AnnouncementIndicatorLevel"]]  * Data$AnnouncementIndicatorLevel[TimePointA] +  
  coef(model)[["AnnouncementTrend"]]           * Data$AnnouncementTrend[TimePointA] +  
  coef(model)[["AnnouncementIndicatorTrend"]]  * Data$AnnouncementIndicatorTrend[TimePointA] +  
  QC.fun("I(AnnouncementIndicatorTrend^2)")    * Data$AnnouncementIndicatorTrend[TimePointA]^2

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - SE
###############################################################################

#CORRELATION
SEAnnouncement <- 
      vcov(model)["SDILAnnouncement","SDILAnnouncement"]                                        * Data$SDILAnnouncement[TimePointA]^2 + 
      vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorLevel"]                    * Data$AnnouncementIndicatorLevel[TimePointA]^2 +
      vcov(model)["AnnouncementTrend","AnnouncementTrend"]                                      * Data$AnnouncementTrend[TimePointA]^2 +  
      vcov(model)["AnnouncementIndicatorTrend","AnnouncementIndicatorTrend"]                    * Data$AnnouncementIndicatorTrend[TimePointA]^2 +  
      CC.fun("I(AnnouncementIndicatorTrend^2)")                                                 * (Data$AnnouncementIndicatorTrend[TimePointA]^2)^2 +
#VARIANCE
      vcov(model)["SDILAnnouncement","AnnouncementIndicatorLevel"]                          * 2 * Data$SDILAnnouncement[TimePointA] * Data$AnnouncementIndicatorLevel[TimePointA] +
      vcov(model)["SDILAnnouncement","AnnouncementTrend"]                                   * 2 * Data$SDILAnnouncement[TimePointA] * Data$AnnouncementTrend[TimePointA] +
      vcov(model)["SDILAnnouncement","AnnouncementIndicatorTrend"]                          * 2 * Data$SDILAnnouncement[TimePointA] * Data$AnnouncementIndicatorTrend[TimePointA] +
      VC.fun(Term="SDILAnnouncement",PowerTerm="I(AnnouncementIndicatorTrend^2)")           * 2 * Data$SDILAnnouncement[TimePointA] * (Data$AnnouncementIndicatorTrend[TimePointA]^2) +
  
      vcov(model)["AnnouncementIndicatorLevel","AnnouncementTrend"]                         * 2 * Data$AnnouncementIndicatorLevel[TimePointA] * Data$AnnouncementTrend[TimePointA] +
      vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorTrend"]                * 2 * Data$AnnouncementIndicatorLevel[TimePointA] * Data$AnnouncementIndicatorTrend[TimePointA] +
      VC.fun(Term="AnnouncementIndicatorLevel",PowerTerm="I(AnnouncementIndicatorTrend^2)") * 2 * Data$AnnouncementIndicatorLevel[TimePointA] * (Data$AnnouncementIndicatorTrend[TimePointA]^2) +
  
      vcov(model)["AnnouncementTrend","AnnouncementIndicatorTrend"]                         * 2 * Data$AnnouncementTrend[TimePointA] * Data$AnnouncementIndicatorTrend[TimePointA] +
      VC.fun(Term="AnnouncementTrend",PowerTerm="I(AnnouncementIndicatorTrend^2)")          * 2 * Data$AnnouncementTrend[TimePointA] * (Data$AnnouncementIndicatorTrend[TimePointA]^2) +
  
      VC.fun(Term="AnnouncementIndicatorTrend",PowerTerm="I(AnnouncementIndicatorTrend^2)") * 2 * Data$AnnouncementIndicatorTrend[TimePointA] * (Data$AnnouncementIndicatorTrend[TimePointA]^2)

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - LOWER CI
###############################################################################
  
LowerCIAnnouncement <- AbsDiffAnnouncement - 1.96 * sqrt(SEAnnouncement)

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - UPPER CI
###############################################################################

UpperCIAnnouncement <- AbsDiffAnnouncement + 1.96 * sqrt(SEAnnouncement)

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - COMBINE RESULTS
###############################################################################

list("Announcement.AbsDiff"          = AbsDiffAnnouncement*(1000/n),
     "Announcement.AbsDiff.LowerCI"  = LowerCIAnnouncement*(1000/n),
     "Announcement.AbsDiff.UpperCI"  = UpperCIAnnouncement*(1000/n))
}

###############################################################################
#FUNCTION FOR ABSOLUTE DIFFERENCES 
#ONE FUNCTION.DIFFERENT FROM PREVIOUS VERSION AS IT CHECKS FOR A QUADRATIC TERM
#SECOND FUNCTION FOR POST-ANNOUNCEMENT TO POST-IMPLEMENTATION
###############################################################################

AbsoluteChangeAnnouncementImplementation.fun <-function(model, n=1, TimePointI=264){
  
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
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION
###############################################################################
  
AbsDiffAnnouncementImplementation <- 
  coef(model)[["SDILAnnouncement"]]             * Data$SDILAnnouncement[TimePointI] + 
  coef(model)[["AnnouncementIndicatorLevel"]]   * Data$AnnouncementIndicatorLevel[TimePointI] +  
  coef(model)[["AnnouncementTrend"]]            * Data$AnnouncementTrend[TimePointI] +  
  coef(model)[["AnnouncementIndicatorTrend"]]   * Data$AnnouncementIndicatorTrend[TimePointI] +  
  QC.fun("I(AnnouncementIndicatorTrend^2)")     * Data$AnnouncementIndicatorTrend[TimePointI]^2 +
  
  coef(model)[["SDILImplementation"]]           * Data$SDILImplementation[TimePointI] + 
  coef(model)[["ImplementationIndicatorLevel"]] * Data$ImplementationIndicatorLevel[TimePointI] + 
  coef(model)[["ImplementationTrend"]]          * Data$ImplementationTrend[TimePointI] +  
  coef(model)[["ImplementationIndicatorTrend"]] * Data$ImplementationIndicatorTrend[TimePointI]

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - SE
###############################################################################
  
#CORRELATION
SEAnnouncementImplementation <- 

  #VARIANCE
  vcov(model)["SDILAnnouncement","SDILAnnouncement"]                                      * Data$SDILAnnouncement[TimePointI]^2 + 
  vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorLevel"]                  * Data$AnnouncementIndicatorLevel[TimePointI]^2 +
  vcov(model)["AnnouncementTrend","AnnouncementTrend"]                                    * Data$AnnouncementTrend[TimePointI]^2 +  
  vcov(model)["AnnouncementIndicatorTrend","AnnouncementIndicatorTrend"]                  * Data$AnnouncementIndicatorTrend[TimePointI]^2 +  
  CC.fun("I(AnnouncementIndicatorTrend^2)")                                               * (Data$AnnouncementIndicatorTrend[TimePointI]^2)^2 +
  vcov(model)["SDILImplementation","SDILImplementation"]                                  * Data$SDILImplementation[TimePointI]^2 + 
  vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorLevel"]              * Data$ImplementationIndicatorLevel[TimePointI]^2 +
  vcov(model)["ImplementationTrend","ImplementationTrend"]                                * Data$ImplementationTrend[TimePointI]^2 +  
  vcov(model)["ImplementationIndicatorTrend","ImplementationIndicatorTrend"]              * Data$ImplementationIndicatorTrend[TimePointI]^2 +  
  
  #COVARIANCE
  vcov(model)["SDILAnnouncement","AnnouncementIndicatorLevel"]                            * 2 * Data$SDILAnnouncement[TimePointI] * Data$AnnouncementIndicatorLevel[TimePointI] +
  vcov(model)["SDILAnnouncement","AnnouncementTrend"]                                     * 2 * Data$SDILAnnouncement[TimePointI] * Data$AnnouncementTrend[TimePointI] +
  vcov(model)["SDILAnnouncement","AnnouncementIndicatorTrend"]                            * 2 * Data$SDILAnnouncement[TimePointI] * Data$AnnouncementIndicatorTrend[TimePointI] +
  VC.fun(Term="SDILAnnouncement",PowerTerm="I(AnnouncementIndicatorTrend^2)")             * 2 * Data$SDILAnnouncement[TimePointI] * (Data$AnnouncementIndicatorTrend[TimePointI]^2) +
  vcov(model)["SDILAnnouncement","SDILImplementation"]                                    * 2 * Data$SDILAnnouncement[TimePointI] * Data$SDILImplementation[TimePointI] +
  vcov(model)["SDILAnnouncement","ImplementationIndicatorLevel"]                          * 2 * Data$SDILAnnouncement[TimePointI] * Data$ImplementationIndicatorLevel[TimePointI] +
  vcov(model)["SDILAnnouncement","ImplementationTrend"]                                   * 2 * Data$SDILAnnouncement[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["SDILAnnouncement","ImplementationIndicatorTrend"]                          * 2 * Data$SDILAnnouncement[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
  
  vcov(model)["AnnouncementIndicatorLevel","AnnouncementTrend"]                           * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * Data$AnnouncementTrend[TimePointI] +
  vcov(model)["AnnouncementIndicatorLevel","AnnouncementIndicatorTrend"]                  * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * Data$AnnouncementIndicatorTrend[TimePointI] +
  VC.fun(Term="AnnouncementIndicatorLevel",PowerTerm="I(AnnouncementIndicatorTrend^2)")   * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * (Data$AnnouncementIndicatorTrend[TimePointI]^2) +
  vcov(model)["AnnouncementIndicatorLevel","SDILImplementation"]                          * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * Data$SDILImplementation[TimePointI] +  
  vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorLevel"]                * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * Data$ImplementationIndicatorLevel[TimePointI] +
  vcov(model)["AnnouncementIndicatorLevel","ImplementationTrend"]                         * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * Data$ImplementationTrend[TimePointI] +  
  vcov(model)["AnnouncementIndicatorLevel","ImplementationIndicatorTrend"]                * 2 * Data$AnnouncementIndicatorLevel[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
  
  vcov(model)["AnnouncementTrend","AnnouncementIndicatorTrend"]                           * 2 * Data$AnnouncementTrend[TimePointI] * Data$AnnouncementIndicatorTrend[TimePointI] +
  VC.fun(Term="AnnouncementTrend",PowerTerm="I(AnnouncementIndicatorTrend^2)")            * 2 * Data$AnnouncementTrend[TimePointI] * (Data$AnnouncementIndicatorTrend[TimePointI]^2) +
  vcov(model)["AnnouncementTrend","SDILImplementation"]                                   * 2 * Data$AnnouncementTrend[TimePointI] * Data$SDILImplementation[TimePointI] +
  vcov(model)["AnnouncementTrend","ImplementationIndicatorLevel"]                         * 2 * Data$AnnouncementTrend[TimePointI] * Data$ImplementationIndicatorLevel[TimePointI] +
  vcov(model)["AnnouncementTrend","ImplementationTrend"]                                  * 2 * Data$AnnouncementTrend[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["AnnouncementTrend","ImplementationIndicatorTrend"]                         * 2 * Data$AnnouncementTrend[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
  
  VC.fun(PowerTerm="I(AnnouncementIndicatorTrend^2)",Term="AnnouncementIndicatorTrend")   * 2 * (Data$AnnouncementIndicatorTrend[TimePointI]^2) * Data$AnnouncementIndicatorTrend[TimePointI] +
  VC.fun(PowerTerm="I(AnnouncementIndicatorTrend^2)",Term="SDILImplementation")           * 2 * (Data$AnnouncementIndicatorTrend[TimePointI]^2) * Data$SDILImplementation[TimePointI] +
  VC.fun(PowerTerm="I(AnnouncementIndicatorTrend^2)",Term="ImplementationIndicatorLevel") * 2 * (Data$AnnouncementIndicatorTrend[TimePointI]^2) * Data$ImplementationIndicatorLevel[TimePointI] +
  VC.fun(PowerTerm="I(AnnouncementIndicatorTrend^2)",Term="ImplementationTrend")          * 2 * (Data$AnnouncementIndicatorTrend[TimePointI]^2) * Data$ImplementationTrend[TimePointI] +
  VC.fun(PowerTerm="I(AnnouncementIndicatorTrend^2)",Term="ImplementationIndicatorTrend") * 2 * (Data$AnnouncementIndicatorTrend[TimePointI]^2) * Data$ImplementationIndicatorTrend[TimePointI] +
  
  vcov(model)["AnnouncementIndicatorTrend","SDILImplementation"]                          * 2 * Data$AnnouncementIndicatorTrend[TimePointI] * Data$SDILImplementation[TimePointI] +
  vcov(model)["AnnouncementIndicatorTrend","ImplementationIndicatorLevel"]                * 2 * Data$AnnouncementIndicatorTrend[TimePointI] * Data$ImplementationIndicatorLevel[TimePointI] +
  vcov(model)["AnnouncementIndicatorTrend","ImplementationTrend"]                         * 2 * Data$AnnouncementIndicatorTrend[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["AnnouncementIndicatorTrend","ImplementationIndicatorTrend"]                * 2 * Data$AnnouncementIndicatorTrend[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
    
  vcov(model)["SDILImplementation","ImplementationIndicatorLevel"]                        * 2 * Data$SDILImplementation[TimePointI] * Data$ImplementationIndicatorLevel[TimePointI] +
  vcov(model)["SDILImplementation","ImplementationTrend"]                                 * 2 * Data$SDILImplementation[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["SDILImplementation","ImplementationIndicatorTrend"]                        * 2 * Data$SDILImplementation[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
  
  vcov(model)["ImplementationIndicatorLevel","ImplementationTrend"]                       * 2 * Data$ImplementationIndicatorLevel[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorTrend"]              * 2 * Data$ImplementationIndicatorLevel[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
  
  vcov(model)["ImplementationTrend","ImplementationIndicatorTrend"]                       * 2 * Data$ImplementationTrend[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI]

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - LOWER CI
###############################################################################

LowerCIAnnouncementImplementation <- AbsDiffAnnouncementImplementation - 1.96 * sqrt(SEAnnouncementImplementation)

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - UPPER CI
###############################################################################

UpperCIAnnouncementImplementation <- AbsDiffAnnouncementImplementation + 1.96 * sqrt(SEAnnouncementImplementation)

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - COMBINE RESULTS
###############################################################################

list("AnnouncementImplementation.AbsDiff"          = AbsDiffAnnouncementImplementation*(1000/n),
     "AnnouncementImplementation.AbsDiff.LowerCI"  = LowerCIAnnouncementImplementation*(1000/n),
     "AnnouncementImplementation.AbsDiff.UpperCI"  = UpperCIAnnouncementImplementation*(1000/n))
}

###############################################################################
#FUNCTION FOR ABSOLUTE DIFFERENCES 
#ONE FUNCTION.DIFFERENT FROM PREVIOUS VERSION AS IT CHECKS FOR A QUADRATIC TERM
#SECOND FUNCTION FOR POST-ANNOUNCEMENT TO POST-IMPLEMENTATION
###############################################################################

AbsoluteChangeImplementation.fun <-function(model, n=1, TimePointI=264){
  
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
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION
###############################################################################

AbsDiffImplementation <- 
  coef(model)[["SDILImplementation"]]               * Data$SDILImplementation[TimePointI] + 
  coef(model)[["ImplementationIndicatorLevel"]]     * Data$ImplementationIndicatorLevel[TimePointI] + 
  coef(model)[["ImplementationTrend"]]              * Data$ImplementationTrend[TimePointI] +  
  coef(model)[["ImplementationIndicatorTrend"]]     * Data$ImplementationIndicatorTrend[TimePointI]

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - SE
###############################################################################

#CORRELATION
SEImplementation <- 
  vcov(model)["SDILImplementation","SDILImplementation"]                                    * Data$SDILImplementation[TimePointI]^2 + 
  vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorLevel"]                * Data$ImplementationIndicatorLevel[TimePointI]^2 +
  vcov(model)["ImplementationTrend","ImplementationTrend"]                                  * Data$ImplementationTrend[TimePointI]^2 +  
  vcov(model)["ImplementationIndicatorTrend","ImplementationIndicatorTrend"]                * Data$ImplementationIndicatorTrend[TimePointI]^2 +  
  #COVARIANCE
  vcov(model)["SDILImplementation","ImplementationIndicatorLevel"]                          * 2 * Data$SDILImplementation[TimePointI] * Data$ImplementationIndicatorLevel[TimePointI] +
  vcov(model)["SDILImplementation","ImplementationTrend"]                                   * 2 * Data$SDILImplementation[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["SDILImplementation","ImplementationIndicatorTrend"]                          * 2 * Data$SDILImplementation[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
    
  vcov(model)["ImplementationIndicatorLevel","ImplementationTrend"]                         * 2 * Data$ImplementationIndicatorLevel[TimePointI] * Data$ImplementationTrend[TimePointI] +
  vcov(model)["ImplementationIndicatorLevel","ImplementationIndicatorTrend"]                * 2 * Data$ImplementationIndicatorLevel[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI] +
    
  vcov(model)["ImplementationTrend","ImplementationIndicatorTrend"]                         * 2 * Data$ImplementationTrend[TimePointI] * Data$ImplementationIndicatorTrend[TimePointI]
  
###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - LOWER CI
###############################################################################

LowerCIImplementation <- AbsDiffImplementation - 1.96 * sqrt(SEImplementation)

###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - UPPER CI
###############################################################################

UpperCIImplementation <- AbsDiffImplementation + 1.96 * sqrt(SEImplementation)
  
###############################################################################
#ABSOLUTE DIFFERENCE BETWEEN COUNTERFACTUAL AND INTERVENTION - COMBINE RESULTS
###############################################################################

list("Implementation.AbsDiff"          = AbsDiffImplementation*(1000/n),
     "Implementation.AbsDiff.LowerCI"  = LowerCIImplementation*(1000/n),
     "Implementation.AbsDiff.UpperCI"  = UpperCIImplementation*(1000/n))
}
