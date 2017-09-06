
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test2 <-read.csv("cawr_abund.csv")
summary(test2)
str(test2)
var(test2[2:5])
mean(test2[2:4])
mean(test2$y.3)

cawr.abund<- csvToUMF("cawr_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cawr.abund)
str(cawr.abund)
#scale all observation covariates (covs of detection)
obsCovs(cawr.abund)= scale (obsCovs(cawr.abund))
#siteCovs(cawr.abund)= scale (siteCovs(cawr.abund))
#select particular site covariates to scale below
   #(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(cawr.abund)
sc[,c(2:9,11,13)] <- scale(sc[, c(2:9,11,13)])
siteCovs(cawr.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.cawr <- pcount(~1 ~1, cawr.abund, mixture="P", K=4)
testNB.cawr <- pcount(~1 ~1, cawr.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.cawr, testNB.cawr)
msTEST.cawr <- modSel(fmsTEST)
msTEST.cawr

?pcount

#detection covariates first
det.null.cawr <- pcount(~1 ~1, cawr.abund, mixture="P", K=15)
det.weather.cawr <- pcount(~ Wind + Sky ~1, cawr.abund, mixture="P", K=15)
det.global.cawr <- pcount(~ Jdate + Wind + Sky + Noise ~1, cawr.abund, mixture="P", K=15)
det.sound.cawr <- pcount(~ Noise + Wind ~1, cawr.abund, mixture="P", K=15)
det.date.cawr <- pcount(~ Jdate ~1, cawr.abund, mixture="P", K=15)
det.detect.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund, mixture="P", K=15)
det.notdate.cawr <-pcount(~ Wind + Sky + Noise ~1, cawr.abund, mixture="P", K=15)

fms <- fitList(det.null.cawr, det.weather.cawr, det.global.cawr,
               det.sound.cawr, det.date.cawr, det.detect.cawr, det.notdate.cawr)
ms1.cawr <- modSel(fms)
ms1.cawr
ms1.cawr@Full
   #OLD summary: null is top model but "sound" is second (wind + noise going forward)
  #NEW summary: with the detection covariates scaled,
       # now detect (date + noise) is first, null is second. date is third. sound is 4th.

##site covariates next

#null detection covariates (aka NOT using the knowledge gained above)
null.cawr <- pcount(~1 ~1, cawr.abund, mixture="P", K=15)
global.cawr <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      , cawr.abund, mixture="P", K=15)
local.cawr <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, cawr.abund, mixture="P", K=15)
lh.cawr <- pcount(~ 1 ~ Ccover + TreeHt + BA + Nsnags, cawr.abund, mixture="P", K=15)
##landscape.cawr <- pcount(~ 1 ~ cov 5 + 6, cawr.abund, mixture="P")
treatment.cawr <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, cawr.abund, mixture="P", K=15)
disturbance.cawr <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT + Herbicide,
                            cawr.abund, K=15)

fms2 <- fitList(null.cawr, global.cawr, local.cawr, lh.cawr, treatment.cawr, disturbance.cawr)
ms2.cawr <- modSel(fms2)
ms2.cawr@Full
ms2.cawr

#more appropriate detection covariates
null2.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund, mixture="P", K=15)
global2.cawr <- pcount(~ Jdate + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       , cawr.abund, mixture="P", K=15)
local2.cawr <- pcount(~ Jdate + Noise ~ BA + Ccover + TreeHt + Ldepth, cawr.abund, mixture="P", K=15)
lh2.cawr <- pcount(~ Jdate + Noise ~ Ccover + TreeHt + BA + Nsnags, cawr.abund, mixture="P", K=15)
#landscape.cawr <- pcount(~ Jdate + Noise ~ cov 5 + 6, cawr.abund, mixture="P", K=15)
treatment2.cawr <- pcount(~ Jdate + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, cawr.abund, mixture="P", K=15)
disturbance2.cawr <- pcount(~ Jdate ~ TimeSinceB + TimeSinceT + Herbicide,
                           cawr.abund, K=15)

fms3 <- fitList(null2.cawr, global2.cawr, local2.cawr, lh2.cawr, treatment2.cawr, disturbance2.cawr)
ms3.cawr <- modSel(fms3)
ms3.cawr@Full
ms3.cawr


#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")
