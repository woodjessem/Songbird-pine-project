
library("unmarked")
setwd("~/GRAD SCHOOL - CLEMSON/Project-Specific/R_Work")

test <-read.csv("piwa_abund.csv")
summary(test)
str(test) #y.4 and Noise.4 and Wind.4 and Sky.4 JDate.4 are factors and shouldn't be

piwa.abund<- csvToUMF("piwa_abund.csv", long = FALSE, type = "unmarkedFramePCount")
    ##type may need to change for occupancy (occuRN, pcountOpen, or whichever used) ##
summary(piwa.abund)

?pcount

null.piwa <- pcount(~1 ~1, piwa.abund)
#global.piwa <- pcount(~ detection covariates 1 + 2 ~ covariates 1 + 2 + 3 + 4, piwa.abund)
#local.piwa<-pcount(~ detection covariates 1 + 2 ~ covariates 2 + 3 + 4, piwa.abund)
#lh.piwa<-pcount(~ det cov ~ covariates 3 + 4, piwa.abund)
#landscape.piwa<-pcount(~ det cov ~ cov 5 + 6, piwa.abund)
#treatment.piwa<-pcount(~ det cov ~ cov 7, piwa.abund)

#fms <- fitList(names of all models)
#ms1.cawr <- modSel(fms)
#ms1.cawr@Full
#ms1.cawr

#see help for package "xlsReadWrite" in old notes, if need be#
#write.table(ms1.cawr@Full, file="C:/Users/path.type",sep="\t")
