#SPP code (classifications, comments) 
# covariates: what seems relevant based on research?
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("prwa_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
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

#test for NB or Poisson - most should use Poisson
testP.prwa <- pcount(~1 ~1, prwa.abund, mixture="P", K=4)
testNB.prwa <- pcount(~1 ~1, prwa.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.prwa, testNB.prwa)
msTEST.prwa <- modSel(fmsTEST)
msTEST.prwa
## __ is best for this species. Change below to correspond!

?pcount

#detection covariates first
det.null.bhnu <- pcount(~1 ~1, bhnu.abund, mixture="P", K=15)
det.weather.bhnu <- pcount(~ Wind + Sky ~1, bhnu.abund, mixture="P", K=15)
det.global.bhnu <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, bhnu.abund, mixture="P", K=15)
det.sound.bhnu <- pcount(~ Noise + Wind ~1, bhnu.abund, mixture="P", K=15)
det.date.bhnu <- pcount(~ Jdate ~1, bhnu.abund, mixture="P", K=15)
det.detect.bhnu <- pcount(~ Jdate + Noise + Time ~1, bhnu.abund, mixture="P", K=15)
det.notdate.bhnu <-pcount(~ Wind + Sky + Noise ~1, bhnu.abund, mixture="P", K=15)
det.time.bhnu <-pcount(~ Time ~1, bhnu.abund, mixture="P",K=15)
det.timing.bhnu <-pcount(~ Time + Jdate ~1, bhnu.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.bhnu, det.weather.bhnu, det.global.bhnu,
                 det.sound.bhnu, det.date.bhnu, det.detect.bhnu, det.notdate.bhnu,
                 det.time.bhnu, det.timing.bhnu)
msDC.ybch <- modSel(fmsDC)
msDC.ybch
#msDC.ybch@Full
#summary: 

det.time.bhnu   #positive relationship w time and 
confint(det.time.bhnu, type="state",method="normal")  #significant
#write.table(msDC.ybch@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
Nnull.ybch <- pcount(~1 ~1
                     ,ybch.abund, mixture="P", K=40)
Nglobal.ybch <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                      + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                       + Parea + ShapeIndex
                      , ybch.abund, mixture="P", K=40)
Nlocal.ybch <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , ybch.abund, mixture="P", K=40)
                          #can only include BA OR CCover
Nlh.ybch <- pcount(~ 1
                    ~ XXXX
                   , ybch.abund, mixture="P", K=40)
Nlandmetrics.ybch <- pcount (~ 1 ~ Parea + ShapeIndex
                             , ybch.abund, mixture="P",K=40)
Nlandscape500.ybch <- pcount(~ 1 ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m
                             , ybch.abund, mixture="P", K=40)
Nlandscape1.ybch <- pcount(~ 1 ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km
                           , ybch.abund, mixture="P", K=40)
Nlandscape5.ybch <- pcount(~ 1 ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                           , ybch.abund, mixture="P", K=40)
                      # - can't use Evergreen&Ag,
                      #+ can't use HighDev&OpenDev together
Nlandscape30.ybch <- pcount(~ 1 ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km
                            , ybch.abund, mixture="P", K=40)
                      #- can't use Protected&Ag together,
                      #- can't use Ag&HighDev together
                      #- can't use Evergreen&Ag together
                      #- can't use HighDev&OpenDev together
                      #- can't use Schrubs&OpenDev together
                      #+ can't use Grass&Ag together
                      #+ can't use Ag&OpenDev together
                      #+ can't use Water&Protected together
                      #+ can't use Schrubs&HighDev together
Ntreatment.ybch <- pcount(~ 1 ~ Treatment + Nthins
                          , ybch.abund, mixture ="P", K=40)
Nmanagement.ybch <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                           , ybch.abund, mixture="P", K=40)
Ndisturbance.ybch <- pcount(~ 1 ~ TimeSinceB + TimeSinceT
                            , ybch.abund, mixture="P", K=40)
Nsiteprod.ybch <- pcount(~ 1 ~ PISoils + FPSiteIndex + NSoilTypes
                         , ybch.abund, mixture="P", K=40)
#Nupstate.ybch <- pcount(~ 1 ~ X + Y + Z, ybch.abund, mixture="P", K=40)

fmsN <- fitList(Nnull.ybch, Nglobal.ybch, Nlocal.ybch, Nlh.ybch,
                Nlandmetrics.ybch,
                Nlandscape500.ybch, Nlandscape1.ybch, Nlandscape5.ybch,
                Nlandscape30.ybch,
                Ntreatment.ybch, Nmanagement.ybch, Ndisturbance.ybch,
                Nsiteprod.ybch, Nupstate.ybch)

msN.ybch <- modSel(fmsN)
msN.ybch@Full
msN.ybch
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates ()
null.prwa <- pcount(~ Jdate ~1, prwa.abund, mixture="P", K=40)
global.prwa <- pcount(~ Jdate
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                       + Parea + ShapeIndex
                       , prwa.abund, mixture="P", K=40)
local.prwa <- pcount(~ Jdate
                     ~ Ccover + TreeHt + Ldepth
                     , prwa.abund, mixture="P", K=40) #can only include BA OR CCover
lh.prwa <- pcount(~ Jdate
                  ~ XXXX
                  , prwa.abund, mixture="P", K=40)
landmetrics.prwa <- pcount (~ Jdate
                          ~ Parea + ShapeIndex
                        , prwa.abund, mixture="P",K=40)
landscape500.prwa <- pcount(~ Jdate
                        ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m
                            , prwa.abund, mixture="P", K=40)
landscape1.prwa <- pcount(~ Jdate
                       ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km
                          , prwa.abund, mixture="P", K=40)
landscape5.prwa <- pcount(~ Jdate
                       ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                          , prwa.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.prwa <- pcount(~ Jdate
                       ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km
                           , prwa.abund, mixture="P", K=40)
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
                         , prwa.abund, mixture ="P", K=40)
management.prwa <- pcount(~ Jdate
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , prwa.abund, mixture="P", K=40)
disturbance.prwa <- pcount(~ Jdate
                           ~ TimeSinceB + TimeSinceT
                           , prwa.abund, mixture="P", K=40)
siteprod.prwa <- pcount(~ Jdate ~ PISoils + FPSiteIndex + NSoilTypes
                        , prwa.abund, mixture="P", K=40)
#upstate.prwa <- pcount(~ Jdate ~ X + Y + Z, prwa.abund, mixture="P", K=40)


fms <- fitList(null.prwa, global.prwa, local.prwa, lh.prwa, landmetrics.prwa,
               landscape500.prwa, landscape1.prwa, landscape5.prwa, landscape30.prwa,
               treatment.prwa, management.prwa, disturbance.prwa,
                siteprod.prwa, upstate.prwa)
ms.prwa <- modSel(fms) #note this does not include
ms.prwa
ms.prwa@Full

landscape5.prwa
#dispersion & abundance summary:

confint(landscape5.prwa, type="state",method="normal")
#summary of output:

#write.table(ms.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_msX.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr


################### paste parboot stuff below when figured out ####
prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates ()
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

#write.table(ms2.prwa@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_ms2.xls",sep="\t")
