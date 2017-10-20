#YBCH (foliage gleaner, shrub nester 1-8' max, scrub habitat, insects)
# covariates: shrub/midstory density, forbs, 
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("ybch_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

ybch.abund<- csvToUMF("ybch_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(ybch.abund)
str(ybch.abund)
#scale all observation covariates (covs of detection)
obsCovs(ybch.abund)= scale (obsCovs(ybch.abund))
#siteCovs(ybch.abund)= scale (siteCovs(ybch.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(ybch.abund)
sc[,c(5:26)] <- scale(sc[, c(5:26)])
siteCovs(ybch.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.ybch <- pcount(~1 ~1, ybch.abund, mixture="P", K=4)
testNB.ybch <- pcount(~1 ~1, ybch.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.ybch, testNB.ybch)
msTEST.ybch <- modSel(fmsTEST)
msTEST.ybch
#Poisson is best for ybch.

?pcount

#detection covariates first
det.null.ybch <- pcount(~1 ~1, ybch.abund, mixture="P", K=15)
det.weather.ybch <- pcount(~ Wind + Sky ~1, ybch.abund, mixture="P", K=15)
det.global.ybch <- pcount(~ Jdate + Wind + Sky + Noise ~1, ybch.abund, mixture="P", K=15)
det.sound.ybch <- pcount(~ Noise + Wind ~1, ybch.abund, mixture="P", K=15)
det.date.ybch <- pcount(~ Jdate ~1, ybch.abund, mixture="P", K=15)
det.detect.ybch <- pcount(~ Jdate + Noise ~1, ybch.abund, mixture="P", K=15)
det.notdate.ybch <-pcount(~ Wind + Sky + Noise ~1, ybch.abund, mixture="P", K=15)

fms <- fitList(det.null.ybch, det.weather.ybch, det.global.ybch,
               det.sound.ybch, det.date.ybch, det.detect.ybch, det.notdate.ybch)
ms1.ybch <- modSel(fms)
ms1.ybch
ms1.ybch@Full
#summary: 1st detect (Jdate + Noise), 2nd is global but @ 2.57

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
null.ybch <- pcount(~1 ~1, ybch.abund, mixture="P", K=15)
global.ybch <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      + HWdens_10 + HWdens_50 + HWdens_100 + FG_herb + FG_shrub + NHW_saplings
                      + NP_over_20cm + Rel_HW2P_canopy + Rel_HW2P_shrubcover
                      , ybch.abund, mixture="P", K=15)
local.ybch <- pcount(~ 1 ~ BA + Ccover +TimeSinceT, ybch.abund, mixture="P", K=15)
lh.ybch <- pcount(~ 1 ~ Ccover + TreeHt + BA + Nsnags, ybch.abund, mixture="P", K=15)
##landscape.ybch <- pcount(~ 1 ~ cov 5 + 6, ybch.abund, mixture="P")
treatment.ybch <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, ybch.abund, mixture="P", K=15)
disturbance.ybch <- pcount(~ 1 ~ TimeSinceB + TimeSinceT,
                           ybch.abund,  mixture = "P", K=15)

fms2 <- fitList(null.ybch, global.ybch, local.ybch, lh.ybch, treatment.ybch, disturbance.ybch)
ms2.ybch <- modSel(fms2)
ms2.ybch@Full
ms2.ybch
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# appropriate detection covariates (Jdate + Noise)
null2.ybch <- pcount(~ Jdate + Noise ~1, ybch.abund, mixture="P", K=20)
global2.ybch <- pcount(~ Jdate + Noise ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + Nburns + Nthins + TimeSinceB + TimeSinceT
                       + HWdens_50 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + LCR
                       , ybch.abund, mixture="P", K=20)
local2.ybch <- pcount(~ Jdate + Noise ~ BA + Ccover + TreeHt + Ldepth, ybch.abund, mixture="P", K=20)
lh2.ybch <- pcount(~ Jdate + Noise ~ BA + Ccover + FG_herb + NHW_saplings, ybch.abund, mixture="P", K=20)
#landscape.ybch <- pcount(~ Jdate + Noise ~ cov 5 + 6, ybch.abund, mixture="P", K=20)
treatment2.ybch <- pcount(~ Jdate + Noise ~ Treatment + Nthins, ybch.abund, mixture ="P", K=20)
management2.ybch <- pcount(~ Jdate + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, ybch.abund, mixture="P", K=15)
disturbance2.ybch <- pcount(~ Jdate + Noise ~ TimeSinceB + TimeSinceT,
                            ybch.abund, mixture = "P", K=20)

fms3 <- fitList(null2.ybch, global2.ybch, local2.ybch, lh2.ybch, treatment2.ybch,
                management2.ybch, disturbance2.ybch)
ms3.ybch <- modSel(fms3)
ms3.ybch
#ms3.ybch@Full
lh2.ybch
local2.ybch
disturbance2.ybch

# 10/11/2017 update: after adding in the 9 new variables,
#  and putting ONE variable in lh model, all of a sudden,
# life history is best (-BA, -CCover, -TimeSinceT, +FG_herb,)
# local is next best, but at 3.30

# 10/20/2017 update: after adding 2 variables and replacing/consolidating some,
#  and adding NHW_saplings (midstory) variable to LH,
# lh2 still best, but local also fits above d2 cutoff!


#see help for package "xlsReadWrite" in old notes, if need be#
write.table(ms3.ybch@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ybch_top_models_ms3.xls",sep="\t")
