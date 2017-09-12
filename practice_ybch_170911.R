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
sc[,c(2:9,11,13)] <- scale(sc[, c(2:9,11,13)])
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


#more appropriate detection covariates (Jdate + Noise)
null2.ybch <- pcount(~ Jdate + Noise ~1, ybch.abund, mixture="P", K=15)
global2.ybch <- pcount(~ Jdate + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       , ybch.abund, mixture="P", K=15)
local2.ybch <- pcount(~ Jdate + Noise ~ BA + Ccover + TreeHt + Ldepth, ybch.abund, mixture="P", K=15)
lh2.ybch <- pcount(~ Jdate + Noise ~ BA + Ccover + TimeSinceT, ybch.abund, mixture="P", K=15)
#landscape.ybch <- pcount(~ Jdate + Noise ~ cov 5 + 6, ybch.abund, mixture="P", K=15)
treatment2.ybch <- pcount(~ Jdate + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, ybch.abund, mixture="P", K=15)
disturbance2.ybch <- pcount(~ Jdate + Noise ~ TimeSinceB + TimeSinceT,
                            ybch.abund, mixture = "P", K=15)

fms3 <- fitList(null2.ybch, global2.ybch, local2.ybch, lh2.ybch, treatment2.ybch, disturbance2.ybch)
ms3.ybch <- modSel(fms3)
ms3.ybch
#ms3.ybch@Full
lh2.ybch
local2.ybch
disturbance2.ybch

#ms3 summary: local is best model (BA, Ccover, TreeHt, Ldepth);
#life history (BA + Ccover) second best model at 0.074;
# next best is null @ 0.416
#   next best is disturbance (TimeSinceStuff) @ 0.711)
# finally, treatment and global above d2.

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.ybch@Full, file="C:/Users/path.type",sep="\t")