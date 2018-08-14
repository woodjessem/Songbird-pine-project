#guilds & figures

library("unmarked")
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")


# NESTING

##    TREE NESTERS  n = 25  #

tree.abund<- csvToUMF("Nesting_tree_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(tree.abund)= scale (obsCovs(tree.abund))
sc <- siteCovs(tree.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(tree.abund) <- sc

landmetrics.tree <- pcount (~ Wind + Noise
                            ~ Parea + ShapeIndex
                            , tree.abund, mixture="P",K=80)

NewData.LandMetrics  <- data.frame(ShapeIndex=0, Parea=seq(min(sc$Parea),max(sc$Parea),length=100))
treeN.est.parea <- predict(landmetrics.tree, type="state",
                             newdata=NewData.LandMetrics,appendData=TRUE)

plot(Predicted~ Parea, data=treeN.est.parea, ylim=c(0,200), type="l", lwd=3,
     xlab="Patch Area", ylab="Est. Tree-nester Abundance")
##95% confidence intervals
lines(lower~ Parea, data=treeN.est.parea,  type="l", lwd=3, col="darkgray")
lines(upper~ Parea, data=treeN.est.parea, type="l", lwd=3, col="darkgray")

##    SHRUB NESTERS  n = 16  #

shrub.abund<- csvToUMF("Nesting_shrub_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(shrub.abund)= scale (obsCovs(shrub.abund))
sc <- siteCovs(shrub.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(shrub.abund) <- sc

lh.shrub <- pcount(~ Jdate + Noise + Time 
                   ~ BA + NHW_saplings + FG_shrub + HW_dens_1050
                   , shrub.abund, mixture="P", K=80)
greenberg.shrub <- pcount(~ Jdate + Noise + Time ~ BA + HW_dens_1050 + Nburns
                          , shrub.abund, mixture="P", K=80)

fms_top.shrubN <- fitList(lh.shrub, greenberg.shrub)
NewData.shrubN  <- data.frame(BA=seq(min(sc$BA),max(sc$BA),length=100),
                              NHW_saplings=0, FG_shrub=0, HW_dens_1050=0,
                              Nburns=0)
shrubN.est.ba <- predict(fms_top.shrubN, type="state", newdata=NewData.shrubN,appendData=TRUE)

plot(Predicted~ BA, data=shrubN.est.ba, ylim=c(0,40), type="l", lwd=3,
     xlab="Basal Area (10-factor)", ylab="Est. Shrub-nester Abundance")
#95% confidence intervals
lines(lower~ BA, data=shrubN.est.ba, type="l", lwd=3, col="gray") #ylim=c(0,10)
lines(upper~ BA, data=shrubN.est.ba, type="l", lwd=3, col="gray") #ylim=c(0,10)


##    GROUND NESTERS  n = 10  #

# do one for evergreen at 1km - single variable, single model

ground.abund<- csvToUMF("Nesting_ground_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(ground.abund)= scale (obsCovs(ground.abund))
sc <- siteCovs(ground.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(ground.abund) <- sc

landscape1.ground <- pcount(~ Jdate
                            ~ Evergreen1km + HighDev1km + Schrubs1km + OpenDev1km
                            , ground.abund, mixture="P", K=80)
local.ground <- pcount(~ Jdate
                       ~ Ccover + TreeHt + Ldepth
                       , ground.abund, mixture="P", K=80)
greenberg.ground <- pcount(~ Jdate ~ Ccover + Nsnags + Nburns + TimeSinceB + Ldepth
                           , ground.abund, mixture="P", K=80)

#evergreen1km
NewData.Landscape1  <- data.frame(HighDev1km=0, Schrubs1km=0, OpenDev1km=0,
                        Evergreen1km=seq(min(sc$Evergreen1km),max(sc$Evergreen1km),length=100))
groundN.est.evergreen1 <- predict(landscape1.ground, type="state",
                           newdata=NewData.Landscape1,appendData=TRUE)

plot(Predicted~ Evergreen1km, data=groundN.est.evergreen1, ylim=c(0,30), type="l", lwd=3,
     xlab="Evergreen habitat within 1km of patch", ylab="Est. Ground-nester Abundance")
##95% confidence intervals
lines(lower~ Evergreen1km, data=groundN.est.evergreen1,  type="l", lwd=3, col="darkgray")
lines(upper~ Evergreen1km, data=groundN.est.evergreen1, type="l", lwd=3, col="darkgray")


#code for multiple top models including variable canopy cover

fms_top.ground <- fitList(local.ground, greenberg.ground)
NewData.gn.ccover  <- data.frame(Ccover=seq(min(sc$Ccover),max(sc$Ccover),length=100),
                        TreeHt=0, Ldepth=0,Nsnags=0, Nburns=0, TimeSinceB=0)
groundN.est.ccover <- predict(fms_top.ground, type="state",
                                 newdata=NewData.gn.ccover,appendData=TRUE)

plot(Predicted~ Ccover, data=groundN.est.ccover, ylim=c(0,40), type="l", lwd=3, xlab="Canopy Cover", ylab="Est. Ground-nester Abundance") ##ylim=c(0,10)
#95% confidence intervals
lines(lower~ Ccover, data=groundN.est.ccover, type="l", lwd=3, col="gray") #ylim=c(0,10)
lines(upper~ Ccover, data=groundN.est.ccover, type="l", lwd=3, col="gray") #ylim=c(0,10)



# FORAGING


## Foliage gleaner n=20 ##
#      landscape 500m   - ag500m

fg.abund<- csvToUMF("Behavior_fg_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(fg.abund)= scale (obsCovs(fg.abund))
sc <- siteCovs(fg.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(fg.abund) <- sc

landscape500.fg <- pcount(~ Wind + Sky + Noise
                          ~ Evergreen500m + OpenDev500m + Schrubs500m + Ag500m
                          , fg.abund, mixture="P", K=80)

NewData.Landscape500  <- data.frame(Schrubs500m=0, OpenDev500m=0, Evergreen500m=0,
                                  Ag500m=seq(min(sc$Ag500m),max(sc$Ag500m),length=100))
foliageF.est.ag500 <- predict(landscape500.fg, type="state",
                                  newdata=NewData.Landscape500,appendData=TRUE)

plot(Predicted~ Ag500m, data=foliageF.est.ag500, ylim=c(0,50), type="l", lwd=3,
     xlab="Agricultural habitat within 500m of patch", ylab="Est. Foliage-gleaner Abundance")
##95% confidence intervals
lines(lower~ Ag500m, data=foliageF.est.ag500,  type="l", lwd=3, col="darkgray")
lines(upper~ Ag500m, data=foliageF.est.ag500, type="l", lwd=3, col="darkgray")


## GROUND FORAGERS    n=27 ##
#    local   - canopy cover

gf.abund<- csvToUMF("Behavior_gf_pcount.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(gf.abund)= scale (obsCovs(gf.abund))
sc <- siteCovs(gf.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(gf.abund) <- sc

local.gf <- pcount(~ Jdate
                   ~ Ccover + TreeHt + Ldepth
                   , gf.abund, mixture="P", K=80)

NewData.LocalGF  <- data.frame(TreeHt=0, Ldepth=0,
                                    Ccover=seq(min(sc$Ccover),max(sc$Ccover),length=100))
groundF.est.ccover <- predict(local.gf, type="state",
                              newdata=NewData.LocalGF,appendData=TRUE)

plot(Predicted~ Ccover, data=groundF.est.ccover, ylim=c(0,150), type="l", lwd=3,
     xlab="Canopy Cover (more open to less open)", ylab="Est. Ground-forager Abundance")
##95% confidence intervals
lines(lower~ Ccover, data=groundF.est.ccover,  type="l", lwd=3, col="darkgray")
lines(upper~ Ccover, data=groundF.est.ccover, type="l", lwd=3, col="darkgray")

###SPECIES - taken from Markdown

#PIWA
piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(piwa.abund)= scale (obsCovs(piwa.abund))
sc <- siteCovs(piwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(piwa.abund) <- sc

landscape5.piwa <- pcount(~ Jdate + Time 
                          ~ Evergreen5km + HighDev5km
                          , piwa.abund, mixture="P", K=40)

#evergreen5km from landscape 5km model
NewData.Evergreen5km  <- data.frame(Evergreen5km= seq(min(sc$Evergreen5km),max(sc$Evergreen5km), length=100), HighDev5km = 0)                             
piwa.est.evergreen5km <- predict(landscape5.piwa, type="state", newdata=NewData.Evergreen5km,appendData=TRUE)

plot(Predicted~ Evergreen5km, data=piwa.est.evergreen5km, ylim=c(0,16), type="l", lwd=3, xlab="Evergreen habitat within 5km of patch", ylab="Est. PIWA Abundance")
##95% confidence intervals
lines(lower~ Evergreen5km, data=piwa.est.evergreen5km,  type="l", lwd=3, col="darkgray")
lines(upper~ Evergreen5km, data=piwa.est.evergreen5km, type="l", lwd=3, col="darkgray")

#PIWA - continued
piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(piwa.abund)= scale (obsCovs(piwa.abund))
sc <- siteCovs(piwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(piwa.abund) <- sc

landscape30.piwa <- pcount(~ Jdate + Time 
                           ~ Evergreen30km + HighDev30km + Protected30km
                           , piwa.abund, mixture="P", K=40)

#High-Dev 30km
NewData.HighDev30km <- data.frame(Evergreen30km=0,
                                  HighDev30km=seq(min(sc$HighDev30km),max(sc$HighDev30km), length=100),
                                  Protected30km=0)

piwa.est.highdev30km <- predict(landscape30.piwa, type="state", newdata=NewData.HighDev30km,appendData=TRUE)

plot(Predicted~ HighDev30km, data=piwa.est.highdev30km, ylim=c(0,15), type="l", lwd=3, xlab="Highly Developed Area within 30km of patch", ylab="Est. PIWA Abundance")
##95% confidence intervals
lines(lower~ HighDev30km, data=piwa.est.highdev30km,  type="l", lwd=3, col="darkgray")
lines(upper~ HighDev30km, data=piwa.est.highdev30km, type="l", lwd=3, col="darkgray")

#PRWA
prwa.abund<- csvToUMF("prwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(prwa.abund)= scale (obsCovs(prwa.abund))
sc <- siteCovs(prwa.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(prwa.abund) <- sc
landscape5.prwa <- pcount(~ Jdate
                          ~ Evergreen5km + Grass5km + HighDev5km + Schrubs5km
                          , prwa.abund, mixture="NB", K=60)

NewData.HighDev5km  <- data.frame(Evergreen5km=0, Grass5km=0, HighDev5km=seq(min(sc$HighDev5km),max(sc$HighDev5km),length=100), Schrubs5km=0)
prwa.est.highdev5 <- predict(landscape5.prwa, type="state",newdata=NewData.HighDev5km,appendData=TRUE)

plot(Predicted~ HighDev5km, data=prwa.est.highdev5, ylim=c(0,4), type="l", lwd=3,
     xlab="Highly developed area within 5km of patch", ylab="Est. PRWA Abundance")
##95% confidence intervals
lines(lower~ HighDev5km, data=prwa.est.highdev5,  type="l", lwd=3, col="darkgray")
lines(upper~ HighDev5km, data=prwa.est.highdev5, type="l", lwd=3, col="darkgray")


#NOBO - one of many
nobo.abund<- csvToUMF("nobo_abund.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(nobo.abund)= scale (obsCovs(nobo.abund))
sc <- siteCovs(nobo.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(nobo.abund) <- sc
treatment.nobo <- pcount(~ Jdate + Wind + Sky + Noise +Time
                         ~ Nburns + Nthins
                         , nobo.abund, mixture ="NB", K=80)  #this one I made Nburns instead of treatment for ones with many many burns

NewData.Nthins  <- data.frame(Nburns= 0, Nthins=seq(min(sc$Nthins),max(sc$Nthins), length=100))
nobo.est.nthins <- predict(treatment.nobo, type="state",
                           newdata=NewData.Nthins,appendData=TRUE)

plot(Predicted~ Nthins, data=nobo.est.nthins, ylim=c(0,10), type="l", lwd=3,
     xlab="Number of thinnings", ylab="Est. NOBO Abundance")
##95% confidence intervals
lines(lower~ Nthins, data=nobo.est.nthins,  type="l", lwd=3, col="darkgray")
lines(upper~ Nthins, data=nobo.est.nthins, type="l", lwd=3, col="darkgray")



#EAWP
eawp.abund<- csvToUMF("eawp_abund.csv", long = FALSE, type = "unmarkedFramePCount")
obsCovs(eawp.abund)= scale (obsCovs(eawp.abund))
sc <- siteCovs(eawp.abund)
sc[,c(5:74)] <- scale(sc[, c(5:74)])
siteCovs(eawp.abund) <- sc
landscape500.eawp <- pcount(~ Wind + Sky
                            ~ Evergreen500m + Ag500m + HighDev500m
                            , eawp.abund, mixture="P", K=40)

local.eawp <- pcount(~ Wind + Sky
                     ~ Ccover + TreeHt + Ldepth
                     , eawp.abund, mixture="P", K=40)

NewData.TreeHt  <- data.frame(TreeHt=seq(min(sc$TreeHt),max(sc$TreeHt), length=100),
                              Ccover=0,Ldepth=0)
eawp.est.treeht <- predict(local.eawp, type="state",
                           newdata=NewData.TreeHt,appendData=TRUE)

plot(Predicted~ TreeHt, data=eawp.est.treeht, ylim=c(0,5), type="l", lwd=3,
     xlab="Tree height", ylab="Est. EAWP Abundance")
##95% confidence intervals
lines(lower~ TreeHt, data=eawp.est.treeht,  type="l", lwd=3, col="darkgray")
lines(upper~ TreeHt, data=eawp.est.treeht, type="l", lwd=3, col="darkgray")