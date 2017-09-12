#INBU (foliage gleaner, shrub-nesting, insects, open woodland habitat & EDGES)
# covariates: grasses, shrub density, within a meter above ground, low branches, tree age
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("inbu_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

inbu.abund<- csvToUMF("inbu_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(inbu.abund)
str(inbu.abund)
#scale all observation covariates (covs of detection)
obsCovs(inbu.abund)= scale (obsCovs(inbu.abund))
#siteCovs(inbu.abund)= scale (siteCovs(inbu.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(inbu.abund)
sc[,c(2:9,11,13)] <- scale(sc[, c(2:9,11,13)])
siteCovs(inbu.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.inbu <- pcount(~1 ~1, inbu.abund, mixture="P", K=4)
testNB.inbu <- pcount(~1 ~1, inbu.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.inbu, testNB.inbu)
msTEST.inbu <- modSel(fmsTEST)
msTEST.inbu
#Poisson is best for INBU.

?pcount

#detection covariates first
det.null.inbu <- pcount(~1 ~1, inbu.abund, mixture="P", K=15)
det.weather.inbu <- pcount(~ Wind + Sky ~1, inbu.abund, mixture="P", K=15)
det.global.inbu <- pcount(~ Jdate + Wind + Sky + Noise ~1, inbu.abund, mixture="P", K=15)
det.sound.inbu <- pcount(~ Noise + Wind ~1, inbu.abund, mixture="P", K=15)
det.date.inbu <- pcount(~ Jdate ~1, inbu.abund, mixture="P", K=15)
det.detect.inbu <- pcount(~ Jdate + Noise ~1, inbu.abund, mixture="P", K=15)
det.notdate.inbu <-pcount(~ Wind + Sky + Noise ~1, inbu.abund, mixture="P", K=15)

fms <- fitList(det.null.inbu, det.weather.inbu, det.global.inbu,
               det.sound.inbu, det.date.inbu, det.detect.inbu, det.notdate.inbu)
ms1.inbu <- modSel(fms)
ms1.inbu
ms1.inbu@Full
#summary: 1st detect (Jdate + Noise), 2nd date (Jdate)
#    next closest is d2.39 and is global model!

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
null.inbu <- pcount(~1 ~1, inbu.abund, mixture="P", K=15)
global.inbu <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      , inbu.abund, mixture="P", K=15)
local.inbu <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, inbu.abund, mixture="P", K=15)
lh.inbu <- pcount(~ 1 ~ TreeHt + Age, inbu.abund, mixture="P", K=15)
##landscape.inbu <- pcount(~ 1 ~ cov 5 + 6, inbu.abund, mixture="P")
treatment.inbu <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, inbu.abund, mixture="P", K=15)
disturbance.inbu <- pcount(~ 1 ~ TimeSinceB + TimeSinceT,
                           inbu.abund,  mixture = "P", K=15)

fms2 <- fitList(null.inbu, global.inbu, local.inbu, lh.inbu, treatment.inbu, disturbance.inbu)
ms2.inbu <- modSel(fms2)
ms2.inbu@Full
ms2.inbu
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#more appropriate detection covariates (Jdate + Noise or just JDate)
null2.inbu <- pcount(~ Jdate + Noise ~1, inbu.abund, mixture="P", K=15)
global2.inbu <- pcount(~ Jdate + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       , inbu.abund, mixture="P", K=15)
local2.inbu <- pcount(~ Jdate + Noise ~ BA + Ccover + TreeHt + Ldepth, inbu.abund, mixture="P", K=15)
lh2.inbu <- pcount(~ Jdate + Noise ~ TreeHt + Age, inbu.abund, mixture="P", K=15)
#landscape.inbu <- pcount(~ Jdate + Noise ~ cov 5 + 6, inbu.abund, mixture="P", K=15)
treatment2.inbu <- pcount(~ Jdate + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, inbu.abund, mixture="P", K=15)
disturbance2.inbu <- pcount(~ Jdate + Noise ~ TimeSinceB + TimeSinceT,
                            inbu.abund, mixture = "P", K=15)

fms3 <- fitList(null2.inbu, global2.inbu, local2.inbu, lh2.inbu, treatment2.inbu, disturbance2.inbu)
ms3.inbu <- modSel(fms3)
ms3.inbu
#ms3.inbu@Full
disturbance2.inbu
confint(disturbance2.inbu,type="state",method="normal")
confint(disturbance2.inbu,type="det",method="normal")

#ms3 summary: disturbance (time since B & T) best model
#  null is second best model but it and rest above d2.

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.inbu@Full, file="C:/Users/path.type",sep="\t")