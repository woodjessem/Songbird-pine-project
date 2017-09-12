#RBWO (cavity nester ~50', bark forager, omnivorous, forest habitat)
# covariates: nsnags, treeht, %hardwood, less forested
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("rbwo_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

rbwo.abund<- csvToUMF("rbwo_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(rbwo.abund)
str(rbwo.abund)
#scale all observation covariates (covs of detection)
obsCovs(rbwo.abund)= scale (obsCovs(rbwo.abund))
#siteCovs(rbwo.abund)= scale (siteCovs(rbwo.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(rbwo.abund)
sc[,c(2:9,11,13)] <- scale(sc[, c(2:9,11,13)])
siteCovs(rbwo.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="P", K=4)
testNB.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.rbwo, testNB.rbwo)
msTEST.rbwo <- modSel(fmsTEST)
msTEST.rbwo
#Poisson is best for rbwo.

?pcount

#detection covariates first
det.null.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="P", K=15)
det.weather.rbwo <- pcount(~ Wind + Sky ~1, rbwo.abund, mixture="P", K=15)
det.global.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~1, rbwo.abund, mixture="P", K=15)
det.sound.rbwo <- pcount(~ Noise + Wind ~1, rbwo.abund, mixture="P", K=15)
det.date.rbwo <- pcount(~ Jdate ~1, rbwo.abund, mixture="P", K=15)
det.detect.rbwo <- pcount(~ Jdate + Noise ~1, rbwo.abund, mixture="P", K=15)
det.notdate.rbwo <-pcount(~ Wind + Sky + Noise ~1, rbwo.abund, mixture="P", K=15)

fms <- fitList(det.null.rbwo, det.weather.rbwo, det.global.rbwo,
               det.sound.rbwo, det.date.rbwo, det.detect.rbwo, det.notdate.rbwo)
ms1.rbwo <- modSel(fms)
ms1.rbwo
ms1.rbwo@Full
#summary: 1st is global; next best is weather (0.25),
# then date, nodate, and null all under d2.00.

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
null.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="P", K=15)
global.rbwo <- pcount(~ 1 ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                      , rbwo.abund, mixture="P", K=15)
local.rbwo <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, rbwo.abund, mixture="P", K=15)
lh.rbwo <- pcount(~ 1 ~ TreeHt + BA + Nsnags, rbwo.abund, mixture="P", K=15)
##landscape.rbwo <- pcount(~ 1 ~ cov 5 + 6, rbwo.abund, mixture="P")
treatment.rbwo <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, rbwo.abund, mixture="P", K=15)
disturbance.rbwo <- pcount(~ 1 ~ TimeSinceB + TimeSinceT,
                           rbwo.abund,  mixture = "P", K=15)
#research.rbwo should include forest habitat on landscape & %hardwood

fms2 <- fitList(null.rbwo, global.rbwo, local.rbwo, lh.rbwo, treatment.rbwo, disturbance.rbwo)
ms2.rbwo <- modSel(fms2)
ms2.rbwo@Full
ms2.rbwo
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#more appropriate detection covariates (Jdate + Noise or just JDate)
null2.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~1, rbwo.abund, mixture="P", K=15)
global2.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + BA + Nsnags
                       + Ccover + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Herbicide
                       , rbwo.abund, mixture="P", K=15)
local2.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ BA + Ccover + TreeHt + Ldepth, rbwo.abund, mixture="P", K=15)
lh2.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ TreeHt + BA + Nsnags, rbwo.abund, mixture="P", K=15)
#landscape.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ cov 5 + 6, rbwo.abund, mixture="P", K=15)
treatment2.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide, rbwo.abund, mixture="P", K=15)
disturbance2.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ TimeSinceB + TimeSinceT,
                            rbwo.abund, mixture = "P", K=15)
#research.rbwo should include forest habitat on landscape & %hardwood

fms3 <- fitList(null2.rbwo, global2.rbwo, local2.rbwo, lh2.rbwo, treatment2.rbwo, disturbance2.rbwo)
ms3.rbwo <- modSel(fms3)
ms3.rbwo
#ms3.rbwo@Full

#ms3 summary: null is best and disturbance next closest but @3.07

#confint(disturbance2.inbu,type="state",method="normal")
#confint(disturbance2.inbu,type="det",method="normal")

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.rbwo@Full, file="C:/Users/path.type",sep="\t")