###############################################################################
#PLOTTING ITS FOR ALL DRINKS
#DAVID PELL
#19/11/2018
###############################################################################
#SET WORKING DIRECTORY
###############################################################################

setwd("/dph/sdil/KWP221")

###############################################################################
#RUN REGRESSION MODELS
###############################################################################

source("/dph/sdil/KWP221/Analysis/Volume/HigherTierVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/LowerTierVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/NoLevyVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/ConfectioneryVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/BottledWaterVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/NoSugarVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/LowSugarVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/AlcoholVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/MilkBasedVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/NASFruitJuiceVolume.R")
source("/dph/sdil/KWP221/Analysis/Volume/PowderVolume.R")

###############################################################################
#RUN PLOTTING FUNCTION
###############################################################################

source("/dph/sdil/KWP221/Analysis/Volume/PlottingFunctionVolume19.R")

###############################################################################
#CREATE IMAGE
###############################################################################

png("VolumePlots1-4.png",pointsize=9, width=200, height=200, res=800, units="mm")

###############################################################################
#CREATE 3 X 2 GRID
###############################################################################

m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.68,0.68,0.2))
par(mar = c(3,5,2,4)+0.1)

###############################################################################
#PLOT HIGHER TIER PURCHASES
###############################################################################

plot.fun(model="HigherTier", colour="red", name="High tier",AxisN=31)
title(main=expression("Higher tier \u22658g of sugar per 100ml"), cex.main=1.3)

###############################################################################
#PLOT LOWER TIER PURCHASES
###############################################################################

plot.fun(model="LowerTier", colour="orange", name="Low tier", AxisN=121)
title(main=expression("Lower tier \u22655g \u2013 \u003C8g of sugar per 100ml"),cex.main=1.3)

###############################################################################
#PLOT NO LEVY PURCHASES
###############################################################################

plot.fun(model="NoLevy", colour="green", name="No levy",AxisN=13)
title(main=expression("No levy drinks containing \u003C5g of sugar per 100ml"), cex.main=1.3)

###############################################################################
#PLOT CONFECTIONERY PURCHASES
###############################################################################

plot.fun(model="Confectionery", colour="chocolate4", name="Confectionery", unit="g", AxisN=41, YAxisLimit=1.2)
title(main=expression("Confectionery"),cex.main=1.3)

###############################################################################
#ADD LEGEND
###############################################################################

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Higher tier \u22658g of sugar per 100ml",
                  "Lower tier \u22655g \u2013 \u003C8g of sugar per 100ml",
                  "No levy drinks containing \u003C5g of sugar per 100ml",
                  "Confectionery","Control (toiletries)", "Pre-announcement counterfactual","Pre-implementation counterfactual"), 
       col=c("red","orange","green","chocolate4","grey70", "black","black"), lwd=2, seg.len=4,
       lty=c("solid","solid","solid","solid","solid","dashed","dotted"), cex=1,
       ncol=2)

###############################################################################
#CLOSE IMAGE
###############################################################################

dev.off()

###############################################################################
#CREATE IMAGE
###############################################################################

png("VolumePlots5-8.png",pointsize=9, width=200, height=200, res=800, units="mm")

###############################################################################
#CREATE 3 X 2 GRID
###############################################################################

m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.68,0.68,0.2))
par(mar = c(3,5,2,4)+0.1)

###############################################################################
#PLOT LOW SUGAR PURCHASES
###############################################################################

plot.fun(model="LowSugar", colour="mediumseagreen", name="Low sugar drinks", AxisN=21)
title(main=expression("Drinks containing > 0 and <5g of sugar per 100ml"),cex.main=1.3)

###############################################################################
#PLOT NO SUGAR PURCHASES
###############################################################################

plot.fun(model="NoSugar", colour="goldenrod", name="No sugar drinks", AxisN=21)
title(main=expression("Drinks containing 0g of sugar per 100ml"),cex.main=1.3)

###############################################################################
#PLOT BOTTLED WATER PURCHASES
###############################################################################

plot.fun(model="BottledWater", colour="blue", name="Bottled water", YAxisLimit=1.2)
title(main=expression("Bottled water"), cex.main=1.3)

###############################################################################
#PLOT ALCOHOL PURCHASES
###############################################################################

plot.fun(model="Alcohol", colour="purple", name="Alcohol",AxisN=13)
title(main=expression("Alcoholic drinks"),cex.main=1.3)

###############################################################################
#ADD LEGEND
###############################################################################

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Drinks containing > 0 and <5g of sugar per 100ml",
                  "Drinks containing 0g of sugar per 100ml",
                  "Bottled water",
                  "Alcoholic drinks","Control (toiletries)", "Pre-announcement counterfactual","Pre-implementation counterfactual"), 
       col=c("mediumseagreen","goldenrod","blue","purple","grey70", "black","black"), lwd=2, seg.len=4,
       lty=c("solid","solid","solid","solid","solid","dashed","dotted"), cex=1,
       ncol=2)

###############################################################################
#CLOSE IMAGE
###############################################################################

dev.off()

###############################################################################
#CREATE IMAGE
###############################################################################

png("VolumePlots9-11.png",pointsize=9, width=200, height=200, res=800, units="mm")

###############################################################################
#CREATE 3 X 2 GRID
###############################################################################

m <- matrix(c(1,1,2,2,0,3,3,0,4,4,4,4),nrow = 3,ncol = 4,byrow = TRUE)
layout(mat = m,heights = c(0.68,0.68,0.2))
par(mar = c(3,5,2,4)+0.1)

###############################################################################
#PLOT MILK AND MILK BASED DRINKS PURCHASES
###############################################################################

plot.fun(model="MilkBased", colour="dodgerblue3", name="Milk drinks",AxisN=13)
title(main=expression("Milk and milk based drinks"),cex.main=1.3)

###############################################################################
#PLOT NAS FRUIT JUICE PURCHASES
###############################################################################

plot.fun(model="NASFruitJuice", colour="plum2", name="Fruit juice",AxisN=31)
title(main=expression("Fruit juice containing no added sugar"), cex.main=1.3)

###############################################################################
#PLOT POWDER PURCHASES
###############################################################################

plot.fun(model="Powder", colour="coral3", name="Drinks made from powder",AxisN=121)
title(main=expression("Drinks made from powder"), cex.main=1.3)

###############################################################################
#ADD LEGEND
###############################################################################

plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend = c("Fruit juice containing no added sugar",
                  "Milk and milk based drinks",
                  "Drinks made from powder",
                  "Pre-announcement counterfactual","Pre-implementation counterfactual"), 
       col=c("plum2","dodgerblue3","coral3","grey70", "black","black"), lwd=2, seg.len=4,
       lty=c("solid","solid","solid","solid","dashed","dotted"), cex=1,
       ncol=2)

###############################################################################
#CLOSE IMAGE
###############################################################################

dev.off()

###############################################################################
#TIDY UP
###############################################################################

# rm(Alcohol,BottledWater,Confectionery,HigherTier,LowSugar,LowerTier,MilkBased,NASFruitJuice,
#    NoLevy,NoSugar,Powder,plot.fun)
