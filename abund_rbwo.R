#RBWO (forest-dwelling, insectivore, cavity-nester, bark-forager) 
# covariates: tree age, treeht, canopy cover, big trees, understory doesn't matter
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("rbwo_abund.csv")
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

rbwo.abund<- csvToUMF("rbwo_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(rbwo.abund) #det at 45 sites!
str(rbwo.abund)
#scale all observation covariates (covs of detection)
obsCovs(rbwo.abund)= scale (obsCovs(rbwo.abund))
#siteCovs(rbwo.abund)= scale (siteCovs(rbwo.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(rbwo.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(rbwo.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="P", K=4)
testNB.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.rbwo, testNB.rbwo)
msTEST.rbwo <- modSel(fmsTEST)
msTEST.rbwo
## P is best for this species.

?pcount

#detection covariates first
det.null.rbwo <- pcount(~1 ~1, rbwo.abund, mixture="P", K=15)
det.weather.rbwo <- pcount(~ Wind + Sky ~1, rbwo.abund, mixture="P", K=15)
det.global.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~1, rbwo.abund, mixture="P", K=15)
det.sound.rbwo <- pcount(~ Noise + Wind ~1, rbwo.abund, mixture="P", K=15)
det.date.rbwo <- pcount(~ Jdate ~1, rbwo.abund, mixture="P", K=15)
det.detect.rbwo <- pcount(~ Jdate + Noise ~1, rbwo.abund, mixture="P", K=15)
det.notdate.rbwo <-pcount(~ Wind + Sky + Noise ~1, rbwo.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.rbwo, det.weather.rbwo, det.global.rbwo,
               det.sound.rbwo, det.date.rbwo, det.detect.rbwo, det.notdate.rbwo)
msDC.rbwo <- modSel(fmsDC)
msDC.rbwo
#msDC.rbwo@Full
#summary: global best, weather one shortly after,
#   but date, notdate, null all under d2.0 (in that order)

#write.table(msDC.rbwo@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/XXX_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
## NOT ADJUSTED FOR RBWO YET LINES 60-117  ##
Nnull.ybch <- pcount(~1 ~1
                     ,ybch.abund, mixture="P", K=40)
Nglobal.ybch <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HWdens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
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

#more appropriate detection covariates (global DC)
null.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~1
                    , rbwo.abund, mixture="P", K=120)
global.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , rbwo.abund, mixture="P", K=120) #FPSiteIndex removed
local.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                     ~ Ccover + TreeHt + Ldepth
                     , rbwo.abund, mixture="P", K=120) #can only include BA OR CCover
lh.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                  ~ TreeHt + Ccover + NP_over_20cm + Rel_HW2P_canopy
                  , rbwo.abund, mixture="P", K=120) #consider adding Rel_HW2P_canopy
landmetrics.rbwo <- pcount (~ Jdate + Wind + Sky + Noise
                          ~ Parea + ShapeIndex
                        , rbwo.abund, mixture="P",K=120)
landscape500.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                        ~ Evergreen500m + HighDev500m + Schrubs500m + Ag500m 
                            , rbwo.abund, mixture="P", K=120)
landscape1.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                       ~ Evergreen1km + HighDev1km + Schrubs1km + Ag1km
                          , rbwo.abund, mixture="P", K=120)
landscape5.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                       ~ Evergreen5km + HighDev5km + Schrubs5km
                          , rbwo.abund, mixture="P", K=120)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                       ~ Evergreen30km + HighDev30km +Protected30km
                           , rbwo.abund, mixture="P", K=120)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                         ~ Treatment + Nthins
                         , rbwo.abund, mixture ="P", K=120)
management.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , rbwo.abund, mixture="P", K=120)
disturbance.rbwo <- pcount(~ Jdate + Wind + Sky + Noise
                           ~ TimeSinceB + TimeSinceT
                           , rbwo.abund, mixture="P", K=120)
siteprod.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ PISoils + NSoilTypes
                        , rbwo.abund, mixture="P", K=120) #FPSiteIndex removed
upstate.rbwo <- pcount(~ Jdate + Wind + Sky + Noise ~ Parea + HighDev5km
                       , rbwo.abund, mixture="P", K=120) #5km was pretty arbitrary


fms <- fitList(null.rbwo, global.rbwo, local.rbwo, lh.rbwo, landmetrics.rbwo,
               landscape500.rbwo, landscape1.rbwo, landscape5.rbwo, landscape30.rbwo,
               treatment.rbwo, management.rbwo, disturbance.rbwo,
                siteprod.rbwo, upstate.rbwo)
ms.rbwo <- modSel(fms) #remember FPSiteIndex removed from global & siteprod
ms.rbwo
ms.rbwo@Full
#summary: null model best, then landscape@1km next best

landscape1.rbwo  #second best model, below d2.0
#dispersion & abundance summary:
#Abundance:
#             Estimate   SE     z P(>|z|)
#(Intercept)     3.067 2.9002 1.057  0.2903
#Evergreen1km    0.133 0.1383 0.962  0.3361
#HighDev1km      0.100 0.0887 1.132  0.2575
#Schrubs1km      0.164 0.1174 1.400  0.1614
#Ag1km           0.340 0.1455 2.335  0.0195   #sig


confint(landscape1.rbwo, type="state",method="normal")
#summary of output:
#                        0.025     0.975
#lam(Int)          -2.61730747 8.7511544
#lam(Evergreen1km) -0.13806352 0.4041726
#lam(HighDev1km)   -0.07343763 0.2743585
#lam(Schrubs1km)   -0.06565919 0.3943797
#lam(Ag1km)         0.05460568 0.6248854   #only one that doesn't cross 0

write.table(ms.rbwo@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/rbwo_top_models_ms.xls",sep="\t")

################### paste parboot stuff below when figured out ####
prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#next best detection covariates (could do this with "weather") haven't yet
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

#write.table(ms2.rbwo@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/rbwo_top_models_ms2.xls",sep="\t")
