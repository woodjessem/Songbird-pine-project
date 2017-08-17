
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test2 <-read.csv("cawr_abund.csv")
summary(test2)
str(test2)

cawr.abund<- csvToUMF("cawr_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(cawr.abund)
#obsCovs(cawr.abund)= scale (obsCovs(cawr.abund))
#siteCovs(cawr.abund)= scale (siteCovs(cawr.abund))

?pcount

#no K and no mixture type set (NB or P or ZIP) yet
null.cawr <- pcount(~1 ~1, cawr.abund)
global.cawr <- pcount(~ Noise + Wind + Sky + Jdate ~ Treatment + BA + Nsnags
                      + Ccover + Ldepth + TreeHt + Age + TimeSinceB + Herbicide
                       , cawr.abund)
local.cawr <- pcount(~ 1 ~ BA + Ccover + TreeHt + Ldepth, cawr.abund)
lh.cawr <- pcount(~ 1 ~ Ccover + TreeHt + BA + Nsnags, cawr.abund)
##landscape.piwa <- pcount(~ 1 ~ cov 5 + 6, cawr.abund)
treatment.cawr <- pcount(~ 1 ~ Treatment + BA + TimeSinceB + Herbicide, cawr.abund)


fms.cawr <- fitList(null.cawr, local.cawr, lh.cawr, treatment.cawr)
ms1.cawr <- modSel(fms.cawr)
ms1.cawr@Full
ms1.cawr

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.piwa@Full, file="C:/Users/path.type",sep="\t")
