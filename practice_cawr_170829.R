
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test2 <-read.csv("cawr_abund.csv")
summary(test2)
str(test2)

cawr.abund<- csvToUMF("cawr_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cawr.abund)
obsCovs(cawr.abund)= scale (obsCovs(cawr.abund))
#siteCovs(cawr.abund)= scale (siteCovs(cawr.abund))
#maybe the following can be used instead:
sc <- siteCovs(cawr.abund)
sc[, 2:7] <- scale(sc[, 2:7])
siteCovs(cawr.abund) <- sc

##select more than one with: ,c(2:7,9)

?pcount

#detection covariates first
det.null.cawr <- pcount(~1 ~1, cawr.abund, mixture="P")
det.weather.cawr <- pcount(~ Wind + Sky ~1, cawr.abund, mixture="P")
det.global.cawr <- pcount(~ Jdate + Wind + Sky + Noise ~1, cawr.abund, mixture="P")
det.sound.cawr <- pcount(~ Noise + Wind ~1, cawr.abund, mixture="P")
det.date.cawr <- pcount(~ Jdate ~1, cawr.abund, mixture="P")
det.detect.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund, mixture="P")
det.notdate.cawr <-pcount(~ Wind + Sky + Noise ~1, cawr.abund, mixture="P")

fms <- fitList(det.null.cawr, det.weather.cawr, det.global.cawr,
               det.sound.cawr, det.date.cawr, det.detect.cawr, det.notdate.cawr)
ms1.cawr <- modSel(fms)
ms1.cawr@Full
ms1.cawr
   #OLD summary: null is top model but "sound" is second (wind + noise going forward)
  #NEW summary: with the detection covariates scaled,
       # now detect (date + noise) is first, null is second. date is third. sound is 4th.

##site covariates next

#null detection covariates
null.cawr <- pcount(~1 ~1, cawr.abund, mixture="P")
global.cawr <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                      , cawr.abund, mixture="P")
local.cawr <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, cawr.abund, mixture="P")
lh.cawr <- pcount(~ 1 ~ Ccover + TreeHt + BA + Nsnags, cawr.abund, mixture="P")
##landscape.cawr <- pcount(~ 1 ~ cov 5 + 6, cawr.abund, mixture="P")
treatment.cawr <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + Herbicide, cawr.abund, mixture="P")

fms2 <- fitList(null.cawr, global.cawr, local.cawr, lh.cawr, treatment.cawr)
ms2.cawr <- modSel(fms2)
ms2.cawr@Full
ms2.cawr


## ## ## ## ## DO NOT RUN, DET COVS NOT SCALED, WRONG DET COV MODEL ## ## ## ## ## ##
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
ms3.cawr
ms3.cawr@Full
coef(ms3.cawr)
#summary: in both of these, the null model is the best model.
# The AIC models are different when used detection covariates, but HIGHER...
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

#more appropriate detection covariates
null2.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund, mixture="P")
global2.cawr <- pcount(~ Jdate + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                       , cawr.abund, mixture="P")
local2.cawr <- pcount(~ Jdate + Noise ~ BA + Ccover + TreeHt + Ldepth, cawr.abund, mixture="P")
lh2.cawr <- pcount(~ Jdate + Noise ~ Ccover + TreeHt + BA, cawr.abund, mixture="P")
#landscape.cawr <- pcount(~ Jdate + Noise ~ cov 5 + 6, cawr.abund, mixture="P")
treatment2.cawr <- pcount(~ Jdate + Noise ~ Treatment + BA + TimeSinceB + Herbicide, cawr.abund, mixture="P")

fms3 <- fitList(null2.cawr, global2.cawr, local2.cawr, lh2.cawr, treatment2.cawr)
#fms3 <- fitList(null2.cawr, local2.cawr, lh2.cawr)
ms3.cawr <- modSel(fms3)
ms3.cawr@Full
ms3.cawr


#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")
