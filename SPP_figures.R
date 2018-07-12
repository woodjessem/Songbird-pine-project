#using model avg package
install.packages("AICcmodavg")
install.packages("unmarked")
library(AICcmodavg)
library(unmarked)

setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

#predict with unmarked    # code from ESA 2014
  #remember, treat differently for those that are factors vs. those that are #s

#general structure:
#fitList of top models (d<2) for SP
#write data frame including all important variables -
        # concat factor (levels) if necessary
      # set interest variable to max/min
      # but set every other variable besides one of interest to 0
#define any factors with categorical names
#predict function on the new matrix, type = state
#plot matrix & 95% CI lines
  #barplot with error bars code if need be


fms_top.hawo <- fitList(fm18.hawo, fm5.hawo, fm17.hawo)
NewData.pine100m  <- data.frame(PineBA = 0,AreaA = 0, 
                                PerceivedSystem=factor("Semi-urban",levels=c("Semi-urban","Recreation","Protected","Agriculture")),
                                Snag=0, PineDensity=0,Pine50mA=0, Pine100mA=seq(1,250, length = 120), Pine500mA=0,TreeDensity=0, TreeBA=0, CanopyCvr=0, CanopyHt=0, YearCat=factor("A",levels=c("A","B")))
hawo.est.pine100m <- predict(fms_top.hawo, type="state", newdata=NewData.pine100m,appendData=TRUE)

plot(Predicted~ Pine100mA, data=hawo.est.pine100m, ylim=c(0,10), type="l", lwd=3, xlab="Pine Habitat within 100m (m^2 x10^-2)", ylab="Est. HAWO Abundance")
##95% confidence intervals
lines(lower~ Pine100mA, data=hawo.est.pine100m,  type="l", lwd=3, col="darkgray")
lines(upper~ Pine100mA, data=hawo.est.pine100m, type="l", lwd=3, col="darkgray") #intercet is high!









#Barplot with error bars

###A function - just copy and paste between the next 6 lines ###
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#### Copy and paste above. Do not edit##

#code below for categorical bar plot data - not yet edited
data.mean<-bhnu.est.matrix$Predicted
data.sd<-bhnu.est.matrix$SE

bar.p <-barplot(data.mean,
                names.arg=c("Semi-urban","Recreation","Protected","Agriculture"),
                ylim = c(0, 10), ylab="Est. BHNU Abundance", xlab="System Type",
                #cex.names = 1.5, cex.axis=1.5, cex.lab=1.5, 
                col="darkblue")

error.bar(bar.p,data.mean,data.sd) #sd