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
sc[,c(5:74)] <- scale(sc[, c(5:74)])
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
det.global.ybch <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, ybch.abund, mixture="P", K=15)
det.sound.ybch <- pcount(~ Noise + Wind ~1, ybch.abund, mixture="P", K=15)
det.date.ybch <- pcount(~ Jdate ~1, ybch.abund, mixture="P", K=15)
det.detect.ybch <- pcount(~ Jdate + Noise + Time ~1, ybch.abund, mixture="P", K=15)
det.notdate.ybch <-pcount(~ Wind + Sky + Noise ~1, ybch.abund, mixture="P", K=15)
det.time.ybch <-pcount(~ Time ~1, ybch.abund, mixture="P",K=15)
det.timing.ybch <-pcount(~ Time + Jdate ~1, ybch.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.ybch, det.weather.ybch, det.global.ybch,
                 det.sound.ybch, det.date.ybch, det.detect.ybch, det.notdate.ybch,
                 det.time.ybch, det.timing.ybch)
msDC.ybch <- modSel(fmsDC)
msDC.ybch
msDC.ybch@Full
#summary: 1st detect (Jdate + Noise +Time), 2nd is global but @ 2.58  #time didn't change

det.detect.ybch   #positive relationship w date and negative with noise positive with time
confint(det.detect.ybch, type="state",method="normal")
write.table(msDC.ybch@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ybch_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above - NOT RUN)
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
Nlandscape30.ybch <- pcount(~ 1 ~ Evergreen30km + Grass30km + HighDev30km + Schrubs30km
                            , ybch.abund, mixture="P", K=40)
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

# appropriate detection covariates (Jdate + Noise + Time )
null.ybch <- pcount(~ Jdate + Noise + Time ~1, ybch.abund, mixture="P", K=40)
global.ybch <- pcount(~ Jdate + Noise + Time
                      ~ Treatment + Herbicide + BA + Ccover
                      + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                      + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings
                      + Rel_HW2P_canopy + PISoils + NSoilTypes
                      + Parea + ShapeIndex
                      , ybch.abund, mixture="P", K=40)  #FPSiteIndex, snags, 
local.ybch <- pcount(~ Jdate + Noise + Time
                     ~ Ccover + TreeHt + Ldepth
                     , ybch.abund, mixture="P", K=40) #can only include BA OR CCover
lh.ybch <- pcount(~ Jdate + Noise + Time
                  ~ BA + FG_herb + FG_shrub + HW_dens_1050 + NHW_saplings + Rel_HW2P_canopy
                  , ybch.abund, mixture="P", K=40)
landmetrics.ybch <- pcount (~ Jdate + Noise + Time
                            ~ Parea + ShapeIndex
                            , ybch.abund, mixture="P",K=40)
landscape500.ybch <- pcount(~ Jdate + Noise + Time
                            ~ Evergreen500m + Grass500m + HighDev500m + Schrubs500m + Ag500m + OpenDev500m
                            , ybch.abund, mixture="P", K=40)
landscape1.ybch <- pcount(~ Jdate + Noise + Time
                          ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km + Ag1km + OpenDev500m
                          , ybch.abund, mixture="P", K=40)
landscape5.ybch <- pcount(~ Jdate + Noise + Time
                          ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                          , ybch.abund, mixture="P", K=40)
# - can't use Evergreen&Ag,
#+ can't use HighDev&OpenDev together
landscape30.ybch <- pcount(~ Jdate + Noise + Time
                           ~ Evergreen30km + Grass30km + Schrubs30km
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
treatment.ybch <- pcount(~ Jdate + Noise + Time
                         ~ Treatment + Nthins
                         , ybch.abund, mixture ="P", K=40)
management.ybch <- pcount(~ Jdate + Noise + Time
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , ybch.abund, mixture="P", K=40)
disturbance.ybch <- pcount(~ Jdate + Noise + Time
                           ~ TimeSinceB + TimeSinceT
                           , ybch.abund, mixture="P", K=40)
siteprod.ybch <- pcount(~ Jdate + Noise + Time ~ PISoils + NSoilTypes
                        , ybch.abund, mixture="P", K=40) #FPSiteIndex
#upstate.ybch <- pcount(~ Jdate + Noise + Time ~ X + Y + Z, ybch.abund, mixture="P", K=40)


fms <- fitList(null.ybch, global.ybch, local.ybch, lh.ybch, landmetrics.ybch,
               landscape500.ybch, landscape1.ybch, landscape5.ybch, landscape30.ybch,
               treatment.ybch, management.ybch, disturbance.ybch,
               siteprod.ybch)
ms.ybch <- modSel(fms) #note this does not include upstate
ms.ybch
#ms.ybch@Full

landscape1.ybch
#dispersion & abundance summary:
#Abundance:
#             Estimate    SE      z P(>|z|)
#(Intercept)   -0.3381 0.734 -0.461  0.6449
#Evergreen1km  -0.0548 0.222 -0.247  0.8047
#Grass1km      -0.0349 0.167 -0.209  0.8344
#HighDev1km    -2.8906 3.343 -0.865  0.3872
#Schrubs1km     0.0799 0.137  0.582  0.5603
#Ag1km         -0.4287 0.247 -1.736  0.0825
#OpenDev500m   -0.0877 0.172 -0.511  0.6094

#Detection:
#            Estimate    SE     z  P(>|z|)
#(Intercept)   -0.675 0.345 -1.96 5.05e-02
#Jdate          0.865 0.215  4.02 5.77e-05
#Noise         -0.720 0.233 -3.09 2.02e-03
#Time           0.139 0.176  0.79 4.30e-01

confint(landscape1.ybch, type="state",method="normal")  #sig negative for HighDev
#summary of output:
#                      0.025      0.975
#lam(Int)          -1.7759269 1.09980672
#lam(Evergreen1km) -0.4892675 0.37962456
#lam(Grass1km)     -0.3620745 0.29229216
#lam(HighDev1km)   -9.4424474 3.66119202  #sig
#lam(Schrubs1km)   -0.1889358 0.34869345
#lam(Ag1km)        -0.9126221 0.05518769
#lam(OpenDev500m)  -0.4242802 0.24882932

landscape5.ybch
confint(landscape5.ybch, type="state",method="normal")

write.table(ms.ybch@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/ybch_top_models_ms.xls",sep="\t")

#quick test between some correlated
testHigh.cawr <- pcount(~Jdate ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmsctest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmsctest)
msT.cawr

################### paste parboot stuff below when figured out ####
ybch.abund<- csvToUMF("ybch_abund.csv", long = FALSE, type = "unmarkedFramePCount")



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

#then, later, all landscape models ranked above these! (except 500m)

#see help for package "xlsReadWrite" in old notes, if need be#