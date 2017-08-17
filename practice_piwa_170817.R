
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test <-read.csv("piwa_abund.csv")
summary(test)
str(test) #y.4 and Noise.4 and Wind.4 and Sky.4 JDate.4 are factors and shouldn't be
#stringsAsFactors=FALSE, but this only works for reading, not for below.

piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(piwa.abund)
#obsCovs(piwa.abund)= scale (obsCovs(piwa.abund))
#siteCovs(piwa.abund)= scale (siteCovs(piwa.abund))

?pcount

#no K and no mixture type set (NB or P or ZIP) yet
null.piwa <- pcount(~1 ~1, piwa.abund)
#global.piwa <- pcount(~ Noise + Wind + Sky + Jdate ~ Treatment + BA + Nsnags
 #                     + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
  #                    , piwa.abund)
#local.piwa <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, piwa.abund)
#lh.piwa <- pcount(~ 1 ~ Ccover + TreeHt + BA, piwa.abund)
##landscape.piwa <- pcount(~ 1 ~ cov 5 + 6, piwa.abund)
#treatment.piwa <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + Herbicide, piwa.abund)


#fms <- fitList(null.piwa, global.piwa, local.piwa, lh.piwa, treatment.piwa)
#ms1.piwa <- modSel(fms)
#ms1.piwa@Full
#ms1.piwa

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.piwa@Full, file="C:/Users/path.type",sep="\t")
