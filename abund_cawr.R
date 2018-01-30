#CAWR (open woodlands, insects, cavity, foliage gleaner - brushy thickets, shrubby) 
# covariates: density (BA not Ccover), Nsnags, midstory (NHW_saplings, midstory), OpenDev

library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("cawr_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

cawr.abund<- csvToUMF("cawr_abund.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cawr.abund)
str(cawr.abund)
#scale all observation covariates (covs of detection)
obsCovs(cawr.abund)= scale (obsCovs(cawr.abund))
#siteCovs(cawr.abund)= scale (siteCovs(cawr.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(cawr.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(cawr.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.cawr <- pcount(~1 ~1, cawr.abund, mixture="P", K=4)
testNB.cawr <- pcount(~1 ~1, cawr.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.cawr, testNB.cawr)
msTEST.cawr <- modSel(fmsTEST)
msTEST.cawr
## P is best for cawr.

?pcount

#detection covariates first
det.null.cawr <- pcount(~1 ~1, cawr.abund, mixture="P", K=15)
det.weather.cawr <- pcount(~ Wind + Sky ~1, cawr.abund, mixture="P", K=15)
det.global.cawr <- pcount(~ Jdate + Wind + Sky + Noise ~1, cawr.abund, mixture="P", K=15)
det.sound.cawr <- pcount(~ Noise + Wind ~1, cawr.abund, mixture="P", K=15)
det.date.cawr <- pcount(~ Jdate ~1, cawr.abund, mixture="P", K=15)
det.detect.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund, mixture="P", K=15)
det.notdate.cawr <-pcount(~ Wind + Sky + Noise ~1, cawr.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.cawr, det.weather.cawr, det.global.cawr,
               det.sound.cawr, det.date.cawr, det.detect.cawr, det.notdate.cawr)
msDC.cawr <- modSel(fmsDC)
msDC.cawr
#msDC.cawr@Full
#summary: detect (Jdate+Noise) best, null next best (0.088),
#    date next (0.303), sound (Noise + Wind) last 1.667

#write.table(msDC.cawr@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/cawr_top_models_msDC.xls",sep="\t")

##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (this was the SECOND best model...not run yet)
Nnull.ybch <- pcount(~1 ~1
                     ,ybch.abund, mixture="P", K=40)
Nglobal.ybch <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                      + Rel_HW2P_canopy + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                      , prwa.abund, mixture="P", K=40)   #FPSiteIndex removed
Nlocal.ybch <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , prwa.abund, mixture="P", K=40)
                          #can only include BA OR CCover
Nlh.ybch <- pcount(~ 1
                    ~ BA + Nsnags + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings
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
Nsiteprod.ybch <- pcount(~ 1 ~ PISoils  + NSoilTypes
                         , ybch.abund, mixture="P", K=40)   #FPSiteIndex removed
Nupstate.ybch <- pcount(~ 1 ~ Ag5km + Parea, ybch.abund, mixture="P", K=40)

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
#CAWR (open woodlands, insects, cavity, foliage gleaner - brushy thickets, shrubby) 
# covariates: density (BA not Ccover), Nsnags, midstory (NHW_saplings, midstory), OpenDev

#quick test between HighDev, Open Dev, and Scrubs
testHigh.cawr <- pcount(~Jdate + Noise ~ HighDev30km, cawr.abund, mixture="P", K=10)
testOpen.cawr <- pcount(~Jdate + Noise ~ OpenDev30km, cawr.abund, mixture="P", K=10)
testSchrubs.cawr <- pcount(~Jdate + Noise ~ Schrubs30km, cawr.abund, mixture="P", K=10)
fmstest <- fitList(testHigh.cawr, testOpen.cawr, testSchrubs.cawr)
msT.cawr <- modSel(fmstest)
msT.cawr

#more appropriate detection covariates (Jdate + Noise best model)
null.cawr <- pcount(~ Jdate + Noise ~1, cawr.abund, mixture="P", K=40)
global.cawr <- pcount(~ Jdate + Noise
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings
                       + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , cawr.abund, mixture="P", K=40)
local.cawr <- pcount(~ Jdate + Noise
                     ~ Ccover + TreeHt + Ldepth
                     , cawr.abund, mixture="P", K=40) #can only include BA OR CCover
lh.cawr <- pcount(~ Jdate + Noise
                  ~ BA + Nsnags + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings
                  , cawr.abund, mixture="P", K=40)
landmetrics.cawr <- pcount (~ Jdate + Noise
                          ~ Parea + ShapeIndex
                        , cawr.abund, mixture="P",K=40)
landscape500.cawr <- pcount(~ Jdate + Noise
                        ~ Evergreen500m + OpenDev500m + HighDev500m + Schrubs500m
                            , cawr.abund, mixture="P", K=40)
landscape1.cawr <- pcount(~ Jdate + Noise
                       ~ Evergreen1km + OpenDev1km + HighDev1km + Schrubs1km
                          , cawr.abund, mixture="P", K=40)
landscape5.cawr <- pcount(~ Jdate + Noise
                       ~ Evergreen5km + OpenDev5km + Schrubs1km
                          , cawr.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.cawr <- pcount(~ Jdate + Noise
                       ~ Evergreen30km + OpenDev30km
                           , cawr.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together   #tested these and Open best fit, Scrubs next
treatment.cawr <- pcount(~ Jdate + Noise
                         ~ Treatment + Nthins
                         , cawr.abund, mixture ="P", K=40)
management.cawr <- pcount(~ Jdate + Noise
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , cawr.abund, mixture="P", K=40)
disturbance.cawr <- pcount(~ Jdate + Noise
                           ~ TimeSinceB + TimeSinceT
                           , cawr.abund, mixture="P", K=40)
siteprod.cawr <- pcount(~ Jdate + Noise ~ PISoils + NSoilTypes
                        , cawr.abund, mixture="P", K=40)  #FPSiteIndex removed
upstate.cawr <- pcount(~ Jdate + Noise
                       ~ Ag5km + Parea, cawr.abund, mixture="P", K=40)

fms <- fitList(null.cawr, global.cawr, local.cawr, lh.cawr, landmetrics.cawr,
               landscape500.cawr, landscape1.cawr, landscape5.cawr, landscape30.cawr,
               treatment.cawr, management.cawr, disturbance.cawr,
                siteprod.cawr, upstate.cawr)
ms.cawr <- modSel(fms) #note this does not include
ms.cawr
ms.cawr@Full

null.cawr # best model

upstate.cawr  #second best model, at 1.14
#dispersion & abundance summary:
#              Estimate  SE     z   P(>|z|)
#(Intercept)    1.302 0.5447  2.39  0.0169
#Ag5km          0.126 0.0974  1.29  0.1967
#Parea         -0.118 0.1164 -1.01  0.3111

#Detection:
#           Estimate    SE      z P(>|z|)
#(Intercept)   -1.191 0.709 -1.679  0.0932
#Jdate         -0.124 0.100 -1.235  0.2168
#Noise          0.112 0.118  0.942  0.3464

confint(upstate.cawr, type="state",method="normal")
#summary of output:
#               0.025     0.975
#lam(Int)    0.23396638 2.3692864
#lam(Ag5km) -0.06517753 0.3167911
#lam(Parea) -0.34611308 0.1102721

write.table(ms.cawr@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/cawr_top_models_ms.xls",sep="\t")

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
