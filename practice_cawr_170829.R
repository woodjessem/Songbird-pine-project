
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test2 <-read.csv("cawr_abund.csv")
summary(test2)
str(test2)

cawr.abund<- csvToUMF("cawr_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cawr.abund)
#obsCovs(cawr.abund)= scale (obsCovs(cawr.abund))
#siteCovs(cawr.abund)= scale (siteCovs(cawr.abund))

?pcount

#detection covariates first
det.null.cawr <- pcount(~1 ~1, cawr.abund)
det.weather.cawr <- pcount(~ Wind + Sky ~1, cawr.abund)
det.global.cawr <- pcount(~ Jdate + Wind + Sky + Noise ~1, cawr.abund)
det.sound.cawr <- pcount(~ Noise + Wind ~1, cawr.abund)
det.date.cawr <- pcount(~ Jdate ~1, cawr.abund)
det.detect.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund)
det.notdate.cawr <-pcount(~ Wind + Sky + Noise ~1, cawr.abund)

fms <- fitList(det.null.cawr, det.weather.cawr, det.global.cawr,
               det.sound.cawr, det.date.cawr, det.detect.cawr, det.notdate.cawr)
ms1.cawr <- modSel(fms)
ms1.cawr@Full
ms1.cawr
   #summary: null is top model but "sound" is second (wind + noise going forward)

##site covariates next

#null detection covariates
#no K and no mixture type set (NB or P or ZIP) yet
null.cawr <- pcount(~1 ~1, cawr.abund)
global.cawr <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                      , cawr.abund)
local.cawr <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, cawr.abund)
lh.cawr <- pcount(~ 1 ~ Ccover + TreeHt + BA + Nsnags, cawr.abund)
##landscape.cawr <- pcount(~ 1 ~ cov 5 + 6, cawr.abund)
treatment.cawr <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + Herbicide, cawr.abund)

fms2 <- fitList(null.cawr, global.cawr, local.cawr, lh.cawr, treatment.cawr)
ms2.cawr <- modSel(fms2)
ms2.cawr@Full
ms2.cawr

#using second best model (wind + noise) detection covariates
#no K and no mixture type set (NB or P or ZIP) yet
null2.cawr <- pcount(~ Wind + Noise ~1, cawr.abund)
global2.cawr <- pcount(~ Wind + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                       , cawr.abund)
local2.cawr <- pcount(~ Wind + Noise ~ BA + Ccover + TreeHt + Ldepth, cawr.abund)
lh2.cawr <- pcount(~ Wind + Noise ~ Ccover + TreeHt + BA, cawr.abund)
#landscape.cawr <- pcount(~ Wind + Noise ~ cov 5 + 6, cawr.abund)
treatment2.cawr <- pcount(~ Wind + Noise ~ Treatment + BA + TimeSinceB + Herbicide, cawr.abund)

fms3 <- fitList(null2.cawr, global2.cawr, local2.cawr, lh2.cawr, treatment2.cawr)
ms3.cawr <- modSel(fms3)
ms3.cawr@Full
ms3.cawr

#summary: in both of these, the null model is the best model.
# The AIC models are different when used detection covariates, but HIGHER...

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")
