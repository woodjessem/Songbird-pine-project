setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

vdata <-read.csv("template_pcount_csv.csv")

library("ggplot2")
library("rmarkdown")
library("markdown")
library("knitr")
library("RColorBrewer")
library("lme4")
library("lmtest")
library("unmarked")
library("AICcmodavg")
library("quantreg")


#canopy cover varied significantly by treatment
ccover.lm<-lm(Ccover~Nburns,data=vdata)
summary(ccover.lm)
confint.lm(ccover.lm)
plot(ccover.lm)

#summary(vdata)
#levels(vdata$Treatment)
#display.brewer.all()
#myColors<-brewer.pal(4,"Greens")
#names(myColors)<-levels(vdata$Treatment)
#colScale<-scale_colour_manual(name="Treatment",values=myColors)
#colScale

#fig+colScale+theme(panel.background = element_rect(fill = "gray",colour = "lightgray",size = 0.5, linetype = "solid"))

#brewer.pal(n=8,name="Greens")
#palette(brewer.pal(n=4,name="Greens"))

fig<-print(ggplot(vdata,aes(x=Nburns,y=Ccover,colour=Treatment))
           +geom_jitter()
           +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
           +labs(x="Increasing # of burns",y="Canopy cover (more dense)")
           +scale_color_brewer(palette="Greens")+theme(panel.background = element_rect(fill = "tan",colour = "lightgray",size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2

#stat_quantile(data=vdata,quantiles=c(0.025,0.975),formula=Ccover~Nburns,method="rq",geom="quantile"))
fig

plot(Ccover~Nburns,data=vdata)
abline(ccover.lm)


## relative hardwood to pine in canopy sig with #burns
hw2p.lm<-lm(Rel_HW2P_canopy~Nburns,data=vdata)
summary(hw2p.lm)
confint.lm(hw2p.lm)

fig2<-print(ggplot(vdata,aes(x=Nburns,y=Rel_HW2P_canopy,colour=Treatment))
           +geom_jitter()
           +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
           +labs(x="Increasing # of burns",y="Relative hardwood to pine (in canopy)")
           +scale_color_brewer(palette="Greens")
           +theme(panel.background = element_rect(fill = "tan",colour = "lightgray",
                                                  size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2
fig2

#litterdepth sig with #burns
litter.lm<-lm(Ldepth~Nburns,data=vdata)
summary(litter.lm)
confint.lm(litter.lm)

fig3<-print(ggplot(vdata,aes(x=Nburns,y=Ldepth,colour=Treatment))
      +geom_jitter()
      +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
      +labs(x="Increasing # of burns",y="Litter depth (cm)")
      +scale_color_brewer(palette="Greens")+theme(panel.background = element_rect(fill = "tan",colour = "lightgray",size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2
fig3


#BA sig neg with #burns (and also treatment 2B, 3B significantly)
baB.lm<-lm(BA~Nburns,data=vdata)
summary(baB.lm)
confint.lm(baB.lm)

fig4<-print(ggplot(vdata,aes(x=Nburns,y=BA,colour=Treatment))
           +geom_jitter()
           +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
           +labs(x="Increasing # of burns",y="Basal Area")
           +scale_color_brewer(palette="Greens")+theme(panel.background = element_rect(fill = "tan",colour = "lightgray",size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2
fig4


# herbaceous cover at low heights sig increase with Nburns
#also with Tx - sig increase for 2B&3B


fgherbB.lm<-lm(FG_herb~Nburns,data=vdata)
summary(fgherbB.lm)
confint.lm(fgherbB.lm)

fig5<-print(ggplot(vdata,aes(x=Nburns,y=FG_herb,colour=Treatment))
           +geom_jitter()
           +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
           +labs(x="Increasing # of burns",y="Herbaceous cover in understory")
           +scale_color_brewer(palette="Greens")+theme(panel.background = element_rect(fill = "tan",colour = "lightgray",size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2
fig5



# HW sapling density at low-med heights sig increase w Nburns
#also sig increase for 2B & 3B with Treatment

hw1050B.lm<-lm(HW_dens_1050~Nburns,data=vdata)
summary(hw1050B.lm)
confint.lm(hw1050B.lm)


fig6<-print(ggplot(vdata,aes(x=Nburns,y=HW_dens_1050,colour=Treatment))
           +geom_jitter()
           +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
           +labs(x="Increasing # of burns",y="Hardwood seedlings in understory")
           +scale_color_brewer(palette="Greens")+theme(panel.background = element_rect(fill = "tan",colour = "lightgray",size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2
fig6



#figures with thinnings... worth it or not?


#NHW_saplings sig! - 
HWsaplingsB.lm<-lm(NHW_saplings~Nburns,data=vdata)
summary(HWsaplingsB.lm)
confint.lm(HWsaplingsB.lm)

fig7<-print(ggplot(vdata,aes(x=Nburns,y=NHW_saplings,colour=Treatment))
           +geom_jitter()
           +geom_smooth(method=lm,fullrange=TRUE,color="black",size=0.5)
           +labs(x="Increasing # of burns",y="Hardwood saplings (midstory)")
           +scale_color_brewer(palette="Greens")+theme(panel.background = element_rect(fill = "tan",colour = "lightgray",size = 0.5, linetype = "solid"))) #or tan1 or lightsalmon1 or navajowhite2
fig7


