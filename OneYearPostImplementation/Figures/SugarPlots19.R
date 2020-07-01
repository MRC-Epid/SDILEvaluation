###############################################################################
#PLOTTING ITS FOR ALL DRINKS
#DAVID PELL
#19/11/2018
###############################################################################
#LIBRARIES
###############################################################################

library("grDevices")
library("devEMF")

###############################################################################
#SET WORKING DIRECTORY
###############################################################################

setwd("/dph/sdil/KWP221")

###############################################################################
#RUN REGRESSION MODELS
###############################################################################

source("/dph/sdil/KWP221/Analysis/Sugar/HigherTierSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/LowerTierSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/NoLevySugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/ConfectionerySugar.R")

source("/dph/sdil/KWP221/Analysis/Sugar/LowSugarSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/MilkBasedSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/NASFruitJuiceSugar.R")
source("/dph/sdil/KWP221/Analysis/Sugar/PowderSugar.R")

###############################################################################
#RUN PLOTTING FUNCTION
###############################################################################

source("/dph/sdil/KWP221/Analysis/Sugar/PlottingFunctionSugar19.R")

###############################################################################
#CREATE IMAGE
###############################################################################

png("SugarPlots1-4.png",pointsize=9, width=200, height=200, res=800, units="mm")

###############################################################################
#CREATE 3 X 2 GRID
###############################################################################

m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.68,0.68,0.2))
par(mar = c(3,5,2,4)+0.1)

###############################################################################
#PLOT HIGHER TIER PURCHASES
###############################################################################

plotSugar.fun(model="HigherTierSugar", colour="red", name="High tier", n=1000, SndYAxisTo=0.5,SndYAxisLength=21)
title(main=expression("Higher tier \u22658g of sugar per 100ml"), cex.main=1.3)

###############################################################################
#PLOT LOWER TIER PURCHASES
###############################################################################

plotSugar.fun(model="LowerTierSugar", colour="orange", name="Low tier", n=100, SndYAxisTo=0.9, SndYAxisLength=19)
title(main=expression("Lower tier \u22655g \u2013 \u003C8g of sugar per 100ml"), cex.main=1.3)

###############################################################################
#PLOT NO LEVY PURCHASES
###############################################################################

plotSugar.fun(model="NoLevySugar", colour="green", name="No levy drinks", n=100, SndYAxisTo=0.9, SndYAxisLength=19)
title(main=expression("No levy drinks \u003C5g of sugar per 100ml"), cex.main=1.3)

###############################################################################
#PLOT CONFECTIONERY PURCHASES
###############################################################################

plotSugar.fun(model="ConfectionerySugar", colour="chocolate4", name="Confectionery", n=1000, SndYAxisTo=0.4,SndYAxisLength=9)
title(main=expression("Confectionery"), cex.main=1.3)

###############################################################################
#ADD LEGEND
###############################################################################

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Higher tier \u22658g of sugar per 100ml",
                  "Lower tier \u22655g \u2013 \u003C8g of sugar per 100ml", 
                  "No levy drinks \u003C5g of sugar per 100ml",
                  "Confectionery","Control (toiletries)", "Pre-announcement counterfactual","Pre-implementation counterfactual"), 
       col=c("red","orange","green","chocolate4","grey70", "black","black"), lwd=2, seg.len=4,
       lty=c("solid","solid","solid","solid","solid","dashed","dotted"), cex=1,
       ncol=2)

###############################################################################
#CLOSE IMAGE
###############################################################################

dev.off()

###############################################################################
#CREATE PDF
###############################################################################

png("SugarPlots5-9.png",pointsize=9, width=200, height=200, res=800, units="mm")

###############################################################################
#CREATE 3 X 2 GRID
###############################################################################

m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.68,0.68,0.2))
par(mar = c(3,5,2,4)+0.1)

###############################################################################
#PLOT LOW SUGAR PURCHASES
###############################################################################

plotSugar.fun(model="LowSugarSugar", colour="mediumseagreen", name="Low sugar drinks", n=100,SndYAxisTo=0.8,SndYAxisLength=21)
title(main=expression("Drinks containing > 0 and <5g of sugar per 100ml"), cex.main=1.3)

###############################################################################
#PLOT MILK BASED PURCHASES
###############################################################################

plotSugar.fun(model="MilkBasedSugar", colour="dodgerblue3", name="Milk drinks",n=1000)
title(main=expression("Milk and milk based drinks"), cex.main=1.3)

###############################################################################
#PLOT NAS FRUIT JUICE PURCHASES
###############################################################################

plotSugar.fun(model="NASFruitJuiceSugar", colour="plum2", name="Fruit juice", n=1000, SndYAxisTo=0.8, SndYAxisLength=41)
title(main=expression("Fruit juice containing no added sugar"), cex.main=1.3)

###############################################################################
#PLOT POWDER PURCHASES
###############################################################################

plotSugar.fun(model="PowderSugar", colour="coral3", name="Drinks made from powder",n=100, SndYAxisTo=0.4,SndYAxisLength=11)
title(main=expression("Drinks made from powder"), cex.main=1.3)

###############################################################################
#ADD LEGEND
###############################################################################

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Drinks containing > 0 and <5g of sugar per 100ml",
                  "Fruit juice containing no added sugar", "Milk and milk based drinks","Drinks made from powder ","Control (toiletries)", "Pre-announcement counterfactual","Pre-implementation counterfactual"), 
       col=c("mediumseagreen","plum2","dodgerblue3","coral3","grey70", "black","black"), lwd=2, seg.len=4,
       lty=c("solid","solid","solid","solid","solid","dashed","dotted"), cex=1,
       ncol=2)

###############################################################################
#CLOSE IMAGE
###############################################################################

dev.off()

###############################################################################
#TIDY UP
###############################################################################

# rm(ConfectionerySugar,HigherTierSugar,LowerTierSugar,MilkBasedSugar,
#    NASFruitJuiceSugar,NoLevySugar,LowSugarSugar,PowderSugar,plotSugar.fun)
