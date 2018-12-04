#PRWA (foliage gleaner, insects, shrub/tree nester 1-45', open wood warbler, ESS/second growth brushy/bushy habitat)
# covariates: grasses, understory growth, midstory shrub density, 
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("prwa_abund.csv")
summary(test)
str(test)
var(test[2:5])
#mean(test[2:4])
mean(test$y.3)

prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(prwa.abund)
str(prwa.abund)
#scale all observation covariates (covs of detection)
obsCovs(prwa.abund)= scale (obsCovs(prwa.abund))
#siteCovs(prwa.abund)= scale (siteCovs(prwa.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(prwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(prwa.abund) <- sc
sc

#test for NB or Poisson - most should use Poisson
testP.prwa <- pcount(~1 ~1, prwa.abund, mixture="P", K=4)
testNB.prwa <- pcount(~1 ~1, prwa.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.prwa, testNB.prwa)
msTEST.prwa <- modSel(fmsTEST)
msTEST.prwa
#NB is best for prwa. Changed below to correspond!

?pcount

#detection covariates first
det.null.prwa <- pcount(~1 ~1, prwa.abund, mixture="NB", K=15)
det.weather.prwa <- pcount(~ Wind + Sky ~1, prwa.abund, mixture="NB", K=15)
det.global.prwa <- pcount(~ Jdate + Wind + Sky + Noise + Time ~1, prwa.abund, mixture="NB", K=15)
det.sound.prwa <- pcount(~ Noise + Wind ~1, prwa.abund, mixture="NB", K=15)
det.date.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=15)
det.detect.prwa <- pcount(~ Jdate + Noise + Time ~1, prwa.abund, mixture="NB", K=15)
det.notdate.prwa <-pcount(~ Wind + Sky + Noise ~1, prwa.abund, mixture="NB", K=15)
det.time.prwa <-pcount(~ Time ~1, prwa.abund, mixture="NB", K=15)
det.timing.prwa <-pcount(~ Time + Jdate ~1, prwa.abund, mixture="NB", K=15)

fmsDC <- fitList(det.null.prwa, det.weather.prwa, det.global.prwa,
               det.sound.prwa, det.date.prwa, det.detect.prwa, det.notdate.prwa,
               det.time.prwa, det.timing.prwa)
msDC.prwa <- modSel(fmsDC)
msDC.prwa
#msDC.prwa@Full
#summary: 1st date, 2nd weather (wind + sky) @ 0.25, 3rd is timing (Date+Time) @ 1.15,
# 4th detect (Jdate + Noise) at 2.03
#    next closest is d2.07 and is notdate.

det.date.prwa
confint(det.time.prwa, type="state",method="normal")
det.weather.prwa
confint(det.weather.prwa, type="state",method="normal")
write.table(msDC.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/prwa_top_models_msDC.xls",sep="\t")

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - not run yet)
Nnull.prwa <- pcount(~1 ~1
                     ,prwa.abund, mixture="NB", K=40)
Nglobal.prwa <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , prwa.abund, mixture="NB", K=40)  #Site Index out
Nlocal.prwa <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , prwa.abund, mixture="NB", K=40)
#can only include BA OR CCover
Nlh.prwa <- pcount(~ 1
                   ~ XXXX
                   , prwa.abund, mixture="NB", K=40)
Nlandmetrics.prwa <- pcount (~ 1 ~ Parea + ShapeIndex
                             , prwa.abund, mixture="NB",K=40)
Nlandscape500.prwa <- pcount(~ 1 ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m
                             , prwa.abund, mixture="NB", K=40)
Nlandscape1.prwa <- pcount(~ 1 ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km
                           , prwa.abund, mixture="NB", K=40)
Nlandscape5.prwa <- pcount(~ 1 ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                           , prwa.abund, mixture="NB", K=40)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
Nlandscape30.prwa <- pcount(~ 1 ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km
                            , prwa.abund, mixture="NB", K=40)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
Ntreatment.prwa <- pcount(~ 1 ~ Treatment + Nthins
                          , prwa.abund, mixture ="NB", K=40)
Nmanagement.prwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                           , prwa.abund, mixture="NB", K=40)
Ndisturbance.prwa <- pcount(~ 1 ~ TimeSinceB + TimeSinceT
                            , prwa.abund, mixture="NB", K=40)
Nsiteprod.prwa <- pcount(~ 1 ~ PISoils + FPSiteIndex + NSoilTypes
                         , prwa.abund, mixture="NB", K=40)
#Nupstate.prwa <- pcount(~ 1 ~ X + Y + Z, prwa.abund, mixture="NB", K=40)

fmsN <- fitList(Nnull.prwa, Nglobal.prwa, Nlocal.prwa, Nlh.prwa,
                Nlandmetrics.prwa,
                Nlandscape500.prwa, Nlandscape1.prwa, Nlandscape5.prwa,
                Nlandscape30.prwa,
                Ntreatment.prwa, Nmanagement.prwa, Ndisturbance.prwa,
                Nsiteprod.prwa)  #no upstate

msN.prwa <- modSel(fmsN)
msN.prwa@Full
msN.prwa
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (JDate first)  #NB
null.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="NB", K=60)
global.prwa <- pcount(~ Jdate
                      ~ Treatment + Herbicide + BA +Ccover
                      + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                      + HW_dens_1050 + FG_herb + NHW_saplings + NP_over_20cm
                      + PISoils  + NSoilTypes
                      + Parea + ShapeIndex
                      , prwa.abund, mixture="NB", K=60)  # FPSiteIndex, Nsnags, Rel_HW2P_canopy 
local.prwa <- pcount(~ Jdate
                     ~ Ccover + TreeHt + Ldepth
                     , prwa.abund, mixture="NB", K=60) #can only include BA OR CCover
lh.prwa <- pcount(~ Jdate
                  ~ Age + FG_herb + HW_dens_1050 + NHW_saplings + NP_over_20cm
                  , prwa.abund, mixture="NB", K=60)
landmetrics.prwa <- pcount (~ Jdate
                            ~ Parea + ShapeIndex
                            , prwa.abund, mixture="NB",K=60)
landscape500.prwa <- pcount(~ Jdate
                            ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m
                            , prwa.abund, mixture="NB", K=60)
landscape1.prwa <- pcount(~ Jdate
                          ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km
                          , prwa.abund, mixture="NB", K=60)
landscape5.prwa <- pcount(~ Jdate
                          ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                          , prwa.abund, mixture="NB", K=60)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.prwa <- pcount(~ Jdate
                           ~ Evergreen30km + Grass30km + Schrubs30km + Protected30km
                           , prwa.abund, mixture="NB", K=60)
#- can't use Protected&Ag together,
#- can't use Ag&HighDev together
#- can't use Evergreen&Ag together
#- can't use HighDev&OpenDev together
#- can't use Schrubs&OpenDev together
#+ can't use Grass&Ag together
#+ can't use Ag&OpenDev together
#+ can't use Water&Protected together
#+ can't use Schrubs&HighDev together
treatment.prwa <- pcount(~ Jdate
                         ~ Treatment + Nthins
                         , prwa.abund, mixture ="NB", K=60)
management.prwa <- pcount(~ Jdate
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , prwa.abund, mixture="NB", K=60)
disturbance.prwa <- pcount(~ Jdate
                           ~ TimeSinceB + TimeSinceT
                           , prwa.abund, mixture="NB", K=60)
siteprod.prwa <- pcount(~ Jdate ~ PISoils + NSoilTypes
                        , prwa.abund, mixture="NB", K=60)
#upstate.prwa <- pcount(~ Jdate ~ X + Y + Z, prwa.abund, mixture="NB", K=40)


fms <- fitList(null.prwa, local.prwa, lh.prwa, landmetrics.prwa,
               landscape500.prwa, landscape1.prwa, landscape5.prwa, landscape30.prwa,
               treatment.prwa, management.prwa, disturbance.prwa,
               siteprod.prwa)
ms.prwa <- modSel(fms) #note this does not include UPSTATE OR GLOBAL (couldnt run)
ms.prwa
#ms.prwa@Full

landscape5.prwa
#dispersion & abundance summary:
#Abundance:
#             Estimate    SE      z P(>|z|)
#(Intercept)    -4.304 1.758 -2.449  0.0143
#Evergreen5km   -0.352 0.505 -0.698  0.4852
#Grass5km       -0.704 0.413 -1.705  0.0882
#HighDev5km     -8.043 3.414 -2.356  0.0185
#Schrubs5km      0.167 0.266  0.626  0.5315

#Detection:
#           Estimate    SE     z P(>|z|)
#(Intercept)   -0.739 0.475 -1.56  0.1199
#Jdate          0.479 0.242  1.98  0.0478

#Dispersion:
#Estimate   SE    z P(>|z|)
#   8.02 38.2 0.21   0.834

confint(landscape5.prwa, type="state",method="normal")
#summary of output:
#                      0.025      0.975
#lam(Int)           -7.7486058 -0.8590123
#lam(Evergreen5km)  -1.3411178  0.6368127
#lam(Grass5km)      -1.5137667  0.1052621
#lam(HighDev5km)   -14.7343936 -1.3512654
#lam(Schrubs5km)    -0.3551819  0.6883344

write.table(ms.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/PRWA_top_models_ms.xls",sep="\t")


#12/04 update added landscape models and 3 now come before life history! exported see below.

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr

################### copied and pasted from parboot help
prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
#data(linetran)
#(dbreaksLine <- c(0, 5, 10, 15, 20))
#lengths <- linetran$Length

#ltUMF <- with(prwa.abund, {
#  unmarkedFrameDS(y = cbind(dc1, dc2, dc3, dc4),
#                  siteCovs = data.frame(Length, area, habitat), dist.breaks = dbreaksLine,
#                  tlength = lengths*1000, survey = "line", unitsIn = "m")
#})
#################################################

# Fit a model
#(fm <- distsamp(~area ~habitat, ltUMF))

# Function returning three fit-statistics.
fitstats <- function(landscape5.prwa) {
  observed <- getY(landscape5.prwa@data)    #abund.prwa inside () not recommended
  expected <- fitted(landscape5.prwa)
  resids <- residuals(landscape5.prwa)
  sse <- sum(resids^2, na.rm=TRUE)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
pb <- parboot(landscape5.prwa, fitstats, nsim=25, report=1)
plot(pb, main="")
###########################

library(AICcmodavg)
library(VGAMdata)
require(unmarked)
obs<- Nmix.chisq(landscape5.prwa)
print(obs, digits.vals = 3)
obs.boot <- Nmix.gof.test(landscape5.prwa, nsim=100)
obs.boot
print(obs.boot, digits.vals = 3, digits.chisq= 3)

Nmix.gof.test(landscape5.prwa, nsim = 5, plot.hist = TRUE, report = NULL)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#just as good detection covariates (weather (Wind+Sky) is next best model @ 0.25)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates (Wind+Sky, second best model @ 0.25)
#modelname+2

fms2 <- fitList(null2.prwa, global2.prwa, local2.prwa, lh2.prwa, landmetrics2.prwa, landscape500_2.prwa, landscape1_2.prwa, 
                landscape5_2.prwa, landscape30_2.prwa, treatment2.prwa, management2.prwa, disturbance2.prwa, siteprod2.prwa, upstate2.prwa)
ms2.prwa <- modSel(fms2) #note this does not include 
ms2.prwa
ms2.prwa@Full

landscape5_2.prwa
#dispersion & abundance summary:

confint(landscape5_2.prwa, type="state",method="normal")
#summary of output:

#write.table(ms2.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/prwa_top_models_ms2.xls",sep="\t")

#see help for package "xlsReadWrite" in old notes, if need be#