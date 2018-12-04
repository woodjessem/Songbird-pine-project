#total SR by counts (unique species)
# covariates: what seems relevant based on research?
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("sr_counts.csv") #SR by sites by counts 1-4 file
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
#mean(test[2:4])
mean(test$y.3)

##first, this is all SR BY COUNT (y.1 thru y.4)  ## NOT ONE OVERALL SR VALUE!! ##

SR.abund<- csvToUMF("sr_counts.csv", long = FALSE, type = "unmarkedFramePCount")
##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(SR.abund)
str(SR.abund)
#scale all observation covariates (covs of detection)
obsCovs(SR.abund)= scale (obsCovs(SR.abund))
#siteCovs(SR.abund)= scale (siteCovs(SR.abund))
#select particular site covariates to scale below
#(note: NOT ALL - not treatment, herbicide, or years ones)
sc <- siteCovs(SR.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(SR.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.SR <- pcount(~1 ~1, SR.abund, mixture="P", K=50)
testNB.SR <- pcount(~1 ~1, SR.abund, mixture="NB", K=50)
fmsTEST <- fitList(testP.SR, testNB.SR)
msTEST.SR <- modSel(fmsTEST)
msTEST.SR
## use Poisson distribution

?pcount

#detection covariates first
det.null.SR <- pcount(~1 ~1, SR.abund, mixture="P", K=150)
det.weather.SR <- pcount(~ Wind + Sky ~1, SR.abund, mixture="P", K=150)
det.global.SR <- pcount(~ Jdate + Wind + Sky + Noise ~1, SR.abund, mixture="P", K=150)
det.sound.SR <- pcount(~ Noise + Wind ~1, SR.abund, mixture="P", K=150)
det.date.SR <- pcount(~ Jdate ~1, SR.abund, mixture="P", K=150)
det.detect.SR <- pcount(~ Jdate + Noise ~1, SR.abund, mixture="P", K=150)
det.notdate.SR <-pcount(~ Wind + Sky + Noise ~1, SR.abund, mixture="P", K=150)

fmsDC <- fitList(det.null.SR, det.weather.SR, det.global.SR,
               det.sound.SR, det.date.SR, det.detect.SR, det.notdate.SR)
msDC.SR <- modSel(fmsDC)
msDC.SR
#msDC.SR@Full
#summary: global is the only model that works... haha ok makes sense!

#write.table(msDC.SR@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/OSR_top_models_msDC.xls",sep="\t")


##site covariates next

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
################################# not run, go to line 121
Nnull.ybch <- pcount(~1 ~1
                     ,ybch.abund, mixture="P", K=40)
Nglobal.ybch <- pcount(~ 1
                       ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                      + Rel_HW2P_canopy + PISoils + FPSiteIndex + NSoilTypes
                       + Parea + ShapeIndex
                      , prwa.abund, mixture="P", K=40)
Nlocal.ybch <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , prwa.abund, mixture="P", K=40)
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

#more appropriate detection covariates (using global DCs)
null.SR <- pcount(~ Jdate + Wind + Sky + Noise ~1, SR.abund, mixture="P", K=40)
global.SR <- pcount(~ Jdate + Wind + Sky + Noise
                      ~ Treatment + Herbicide + BA + Nsnags +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + NP_over_20cm
                       + Rel_HW2P_canopy + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , SR.abund, mixture="P", K=40)  #FPSiteIndex removed for now
local.SR <- pcount(~ Jdate + Wind + Sky + Noise
                     ~ Ccover + TreeHt + Ldepth
                     , SR.abund, mixture="P", K=40) #can only include BA OR CCover
lh.SR <- pcount(~ Jdate + Wind + Sky + Noise
                  ~ XXXX
                  , SR.abund, mixture="P", K=40)
landmetrics.SR <- pcount (~ Jdate + Wind + Sky + Noise
                          ~ Parea + ShapeIndex
                        , SR.abund, mixture="P",K=40)
landscape500.SR <- pcount(~ Jdate + Wind + Sky + Noise
                        ~ Evergreen500m + Grass500m + HighDev500m + Ag500m + Schrubs500m + OpenDev500m
                            , SR.abund, mixture="P", K=40)
landscape1.SR <- pcount(~ Jdate + Wind + Sky + Noise
                       ~ Evergreen1km + Grass1km + HighDev1km + Schrubs1km + Ag1km + OpenDev1km
                          , SR.abund, mixture="P", K=40)
landscape5.SR <- pcount(~ Jdate + Wind + Sky + Noise
                       ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                          , SR.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.SR <- pcount(~ Jdate + Wind + Sky + Noise
                       ~ Evergreen30km + Grass30km + HighDev30km + Protected30km
                           , SR.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.SR <- pcount(~ Jdate + Wind + Sky + Noise
                         ~ Treatment + Nthins
                         , SR.abund, mixture ="P", K=40)
management.SR <- pcount(~ Jdate + Wind + Sky + Noise
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , SR.abund, mixture="P", K=40)
disturbance.SR <- pcount(~ Jdate + Wind + Sky + Noise
                           ~ TimeSinceB + TimeSinceT
                           , SR.abund, mixture="P", K=40)
siteprod.SR <- pcount(~ Jdate + Wind + Sky + Noise
                        ~ PISoils + NSoilTypes
                        , SR.abund, mixture="P", K=40)  #FPSiteIndex removed for now
#upstate.SR <- pcount(~ Jdate + Wind + Sky + Noise ~ X + Y + Z, SR.abund, mixture="P", K=40)


fms <- fitList(null.SR, global.SR, local.SR, landmetrics.SR,
               landscape500.SR, landscape1.SR, landscape5.SR, landscape30.SR,
               treatment.SR, management.SR, disturbance.SR,
                siteprod.SR)
ms.SR <- modSel(fms) #note this does not include upstate or lh (right now)
ms.SR
ms.SR@Full

#null is best, local is next,  disturbance is above the 2.0 cutoff (treatment up there too!)
local.SR
#dispersion & abundance summary:
#Abundance:
#            Estimate     SE      z   P(>|z|)
#(Intercept)   3.3251 0.0936 35.508 3.64e-276
#Ccover       -0.0464 0.0302 -1.535  1.25e-01
#TreeHt        0.0377 0.0307  1.230  2.19e-01
#Ldepth       -0.0286 0.0321 -0.891  3.73e-01

#Detection:
#  Estimate     SE      z  P(>|z|)
#(Intercept)   0.1725 0.2006  0.860 3.90e-01
#Jdate        -0.2051 0.0424 -4.839 1.30e-06
#Wind         -0.0840 0.0388 -2.167 3.02e-02
#Sky          -0.0578 0.0332 -1.744 8.12e-02
#Noise        -0.0214 0.0404 -0.531 5.96e-01

confint(local.SR, type="state",method="normal")
#summary of output:
#               0.025      0.975
#lam(Int)     3.14153876 3.50860743         #sig
#lam(Ccover) -0.10570023 0.01284660
#lam(TreeHt) -0.02241896 0.09789730
#lam(Ldepth) -0.09143727 0.03429546
                                     #   everything overlaps 0

write.table(ms.SR@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/OSR_top_models_ms.xls",sep="\t")


######################################################below not run...

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

