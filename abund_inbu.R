#INBU (foliage gleaner, shrub-nesting, insects, open woodland habitat & EDGES)
# covariates: grasses, shrub density, within a meter above ground, low branches, tree age
library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

test <-read.csv("inbu_abund.csv") #spp CSV file!
summary(test)
str(test)
var(test[2:5])  #what were these testing for? variance>mean?
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
sc[,c(5:74)] <- scale(sc[, c(5:74)]) #from 26 to 74 +landscape+soils
siteCovs(inbu.abund) <- sc

#test for NB or Poisson - most should use Poisson
testP.inbu <- pcount(~1 ~1, inbu.abund, mixture="P", K=4)
testNB.inbu <- pcount(~1 ~1, inbu.abund, mixture="NB", K=4)
fmsTEST <- fitList(testP.inbu, testNB.inbu)
msTEST.inbu <- modSel(fmsTEST)
msTEST.inbu
## P is best for this species.

?pcount

#detection covariates first
det.null.inbu <- pcount(~1 ~1, inbu.abund, mixture="P", K=15)
det.weather.inbu <- pcount(~ Wind + Sky ~1, inbu.abund, mixture="P", K=15)
det.global.inbu <- pcount(~ Jdate + Wind + Sky + Noise +Time ~1, inbu.abund, mixture="P", K=15)
det.sound.inbu <- pcount(~ Noise + Wind ~1, inbu.abund, mixture="P", K=15)
det.date.inbu <- pcount(~ Jdate ~1, inbu.abund, mixture="P", K=15)
det.detect.inbu <- pcount(~ Jdate + Noise + Time ~1, inbu.abund, mixture="P", K=15)
det.notdate.inbu <-pcount(~ Wind + Sky + Noise ~1, inbu.abund, mixture="P", K=15)
det.time.inbu <-pcount(~ Time ~1, inbu.abund, mixture="P",K=15)
det.timing.inbu <-pcount(~ Time + Jdate ~1, inbu.abund, mixture="P", K=15)

fmsDC <- fitList(det.null.inbu, det.weather.inbu, det.global.inbu,
                 det.sound.inbu, det.date.inbu, det.detect.inbu, det.notdate.inbu,
                 det.time.inbu, det.timing.inbu)
msDC.inbu <- modSel(fmsDC)
msDC.inbu
#msDC.inbu@Full
#summary: date best but detect (Date+Noise+Time) right after
#    Timing (Time+Date) also under d2.0

det.date.inbu   #positive relationship w time and 
confint(det.date.inbu, type="state",method="normal")

det.detect.inbu
confint(det.detect.inbu, type="state",method="normal")
write.table(msDC.inbu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/inbu_top_models_msDC.xls",sep="\t")

predict(det.detect.inbu, type="det")  #gave me 204 rows...
#backTransform(det.detect.inbu, "psi", method=normal)



##site covariates next

#INBU (foliage gleaner, shrub-nesting, insects, open woodland habitat & EDGES)
# covariates: grasses, shrub density, within a meter above ground, low branches, tree age
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#null detection covariates (aka NOT using the knowledge gained above)
Nnull.inbu <- pcount(~1 ~1
                     ,inbu.abund, mixture="P", K=40)
Nglobal.inbu <- pcount(~ 1
                       ~ Treatment + Herbicide + BA +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                      , inbu.abund, mixture="P", K=40)
#doesn't include FPSiteIndex + NP_over_20cm + Rel_HW2P_canopy + Nsnags
Nlocal.inbu <- pcount(~ 1
                      ~ Ccover + TreeHt + Ldepth
                      , inbu.abund, mixture="P", K=40)
                          #can only include BA OR CCover
Nlh.inbu <- pcount(~ 1
                    ~ HW_dens_1050 + FG_herb + FG_shrub + Age + NHW_saplings
                   , inbu.abund, mixture="P", K=40)
Nlandmetrics.inbu <- pcount (~ 1 ~ Parea + ShapeIndex
                             , inbu.abund, mixture="P",K=40)
Nlandscape500.inbu <- pcount(~ 1 ~ Evergreen500m + Grass500m + Ag500m + HighDev500m + Schrubs500m
                             , inbu.abund, mixture="P", K=40)
Nlandscape1.inbu <- pcount(~ 1 ~ Evergreen1km + Grass1km + Ag1km + HighDev1km + Schrubs1km
                           , inbu.abund, mixture="P", K=40)
Nlandscape5.inbu <- pcount(~ 1 ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                           , inbu.abund, mixture="P", K=40)
                      # - can't use Evergreen&Ag,
                      #+ can't use HighDev&OpenDev together
Nlandscape30.inbu <- pcount(~ 1 ~ Evergreen30km + Grass30km + Schrubs30km + Protected30km
                            , inbu.abund, mixture="P", K=40)
                      #- can't use Protected&Ag together,
                      #- can't use Ag&HighDev together
                      #- can't use Evergreen&Ag together
                      #- can't use HighDev&OpenDev together
                      #- can't use Schrubs&OpenDev together
                      #+ can't use Grass&Ag together
                      #+ can't use Ag&OpenDev together
                      #+ can't use Water&Protected together
                      #+ can't use Schrubs&HighDev together
Ntreatment.inbu <- pcount(~ 1 ~ Treatment + Nthins
                          , inbu.abund, mixture ="P", K=40)
Nmanagement.inbu <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                           , inbu.abund, mixture="P", K=40)
Ndisturbance.inbu <- pcount(~ 1 ~ TimeSinceB + TimeSinceT
                            , inbu.abund, mixture="P", K=40)
Nsiteprod.inbu <- pcount(~ 1 ~ PISoils + NSoilTypes
                         , inbu.abund, mixture="P", K=40)
#Nupstate n/a no data

fmsN <- fitList(Nnull.inbu, Nglobal.inbu, Nlocal.inbu, Nlh.inbu,
                Nlandmetrics.inbu,
                Nlandscape500.inbu, Nlandscape1.inbu, Nlandscape5.inbu,
                Nlandscape30.inbu,
                Ntreatment.inbu, Nmanagement.inbu, Ndisturbance.inbu,
                Nsiteprod.inbu)
                         #note this does not include upstate or some of site.prod
msN.inbu <- modSel(fmsN) 
#msN.inbu@Full
msN.inbu
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#more appropriate detection covariates (Jdate best model)
null.inbu <- pcount(~ Jdate ~1, inbu.abund, mixture="P", K=40)
global.inbu <- pcount(~ Jdate
                      ~ Treatment + Herbicide + BA +Ccover
                       + Ldepth + TreeHt + Age + TimeSinceB + TimeSinceT + Nthins
                       + HW_dens_1050 + FG_herb + FG_shrub + NHW_saplings
                       + PISoils + NSoilTypes
                       + Parea + ShapeIndex
                       , inbu.abund, mixture="P", K=40)
#doesn't include FPSiteIndex + NP_over_20cm + Rel_HW2P_canopy + Nsnags
local.inbu <- pcount(~ Jdate
                     ~ Ccover + TreeHt + Ldepth
                     , inbu.abund, mixture="P", K=40) #can only include BA OR CCover
lh.inbu <- pcount(~ Jdate
                  ~ HW_dens_1050 + FG_herb + FG_shrub + Age + NHW_saplings
                  , inbu.abund, mixture="P", K=40)
landmetrics.inbu <- pcount (~ Jdate
                          ~ Parea + ShapeIndex
                        , inbu.abund, mixture="P",K=40)
landscape500.inbu <- pcount(~ Jdate
                        ~ Evergreen500m + Grass500m + Ag500m + HighDev500m + Schrubs500m
                            , inbu.abund, mixture="P", K=40)
landscape1.inbu <- pcount(~ Jdate
                       ~ Evergreen1km + Grass1km + Ag1km + HighDev1km + Schrubs1km
                          , inbu.abund, mixture="P", K=40)
landscape5.inbu <- pcount(~ Jdate
                       ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                          , inbu.abund, mixture="P", K=40)
                  # - can't use Evergreen&Ag,
                  #+ can't use HighDev&OpenDev together
landscape30.inbu <- pcount(~ Jdate
                       ~ Evergreen30km + Grass30km + Schrubs30km + Protected30km
                           , inbu.abund, mixture="P", K=40)
                  #- can't use Protected&Ag together,
                  #- can't use Ag&HighDev together
                  #- can't use Evergreen&Ag together
                  #- can't use HighDev&OpenDev together
                  #- can't use Schrubs&OpenDev together
                  #+ can't use Grass&Ag together
                  #+ can't use Ag&OpenDev together
                  #+ can't use Water&Protected together
                  #+ can't use Schrubs&HighDev together
treatment.inbu <- pcount(~ Jdate
                         ~ Treatment + Nthins
                         , inbu.abund, mixture ="P", K=40)
management.inbu <- pcount(~ Jdate
                          ~ Treatment + BA + TimeSinceB + TimeSinceT + Herbicide
                          , inbu.abund, mixture="P", K=40)
disturbance.inbu <- pcount(~ Jdate
                           ~ TimeSinceB + TimeSinceT
                           , inbu.abund, mixture="P", K=40)
siteprod.inbu <- pcount(~ Jdate ~ PISoils + NSoilTypes
                        , inbu.abund, mixture="P", K=40)
#upstate n/a no data


fms <- fitList(null.inbu, global.inbu, local.inbu, lh.inbu, landmetrics.inbu,
               landscape500.inbu, landscape1.inbu, landscape5.inbu, landscape30.inbu,
               treatment.inbu, management.inbu, disturbance.inbu,
                siteprod.inbu)
ms.inbu <- modSel(fms) #note this does not include upstate or some of site.prod
ms.inbu
#ms.inbu@Full

#summary: disturbance best! life history next! treatment third - THIS IS AWESOME!!!

disturbance.inbu
#Abundance:
#            Estimate    SE      z  P(>|z|)
#(Intercept)    0.527 0.150  3.518 0.000435
#TimeSinceB    -0.328 0.153 -2.138 0.032529
#TimeSinceT    -0.093 0.166 -0.559 0.576082

#Detection:
#          Estimate    SE       z  P(>|z|)
#(Intercept)  -0.0121 0.246 -0.0491 9.61e-01
#Jdate         0.6208 0.149  4.1536 3.27e-05

confint(disturbance.inbu, type="state",method="normal")
#                    0.025       0.975
#lam(Int)         0.2331732  0.81996178
#lam(TimeSinceB) -0.6281164 -0.02726671        #sig
#lam(TimeSinceT) -0.4190053  0.23300655

backTransform(linearComb(disturbance.inbu, coefficients = c(1,0), type= "det"))
#YAY!
backTransform(linearComb(disturbance.inbu, coefficients = c(1,0,0), type= "state"))

#or:
newData.inbu<-data.frame(TimeSinceT=0, TimeSinceB=0:20)
round(predict(disturbance.inbu, type="state", newdata=newData.inbu, appendData=TRUE, 2))
#or "det"
#confint(disturbance.inbu,type="det")

lh.inbu
confint(lh.inbu, type="state",method="normal")

treatment.inbu
confint(treatment.inbu, type="state",method="normal")

write.table(ms.inbu@Full, file="C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds/inbu_top_models_ms.xls",sep="\t")



library(AICcmodavg)
#testing predict function

NewData.TsinceB  <- data.frame(TimeSinceT= 0, TimeSinceB=seq(0,20, length=100))
inbu.est.timeburn <- predict(disturbance.inbu, type="state",
                                newdata=NewData.TsinceB,appendData=TRUE)

plot(Predicted~ TimeSinceB, data=inbu.est.timeburn, ylim=c(0,8), type="l", lwd=3,
     xlab="Time Since Burn (years)", ylab="Est. INBU Abundance")
##95% confidence intervals
lines(lower~ TimeSinceB, data=inbu.est.timeburn,  type="l", lwd=3, col="darkgray")
lines(upper~ TimeSinceB, data=inbu.est.timeburn, type="l", lwd=3, col="darkgray")





#quick test between some correlated  #not run for inbu 
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
