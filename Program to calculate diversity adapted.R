##############################################################################################
##  Calculations of various diversity indices, species richness estimators and rarefaction  ##
##  For Community Ecology 2016    - adapted by Jesse in Feb 2018 for project                                                          ##
##############################################################################################

#First set the working director where your data are stored and where you want the output to go
setwd("C:/Users/woodj/Documents/GRAD SCHOOL - CLEMSON/Project-Specific/R work/USDA-songbirds/USDA-songbirds")

#Then read in your file you have saved as a comma-delimited file. It will automatically assign the first
# line as column names (variables)
D<-read.csv(file="pcdatac3.csv") #Jesse data on point counts version from 02-02-18 manipulating

#Load the 'vegan' package into R so that its functions will be accessible
library(vegan)

#Creates a Site by Species matrix with Sites as rows
d<-table(D$Site,D$Species)

#Create a vector that has names of the sites from the row names
Sites<-row.names(d)

# Calculate species richness (Hill=0), exponential of Shannon's index of diversity (Hill=1), 
# and the inverse of Simpson's diversity index (Hill=2)
hillnos<-renyi(d,scales=c(0,1,2), hill=T) #Gives species richness, exp(H), invSimpson

#name the columns so that you can act on them
colnames(hillnos)<-c("SpeciesRichness","ExpShannon","InverseSimpson")

# Calculate Shannon evenness (H/ln(S))
hillnos$Sheven<-(log(hillnos$ExpShannon)/log(hillnos$SpeciesRichness)) 

#Calculate Simpson's evenness ((1/D)/S)
hillnos$Simpeven<-hillnos$InverseSimpson/hillnos$SpeciesRichness

#Calculate Fisher's alpha
hillnos$alpha <-fisher.alpha(d)

#Calculate the number of individuals in each site
hillnos$Numinds=rowSums(d)

#Determine the number of individuals found in the Site with the fewest (minimum) individuals
#This number will be needed for rarefaction
minNum<-min(rowSums(d))
#Output this number and write it down to label the number of rarefied species column (E#) in Table 1
minNum

#Calculcate the rarefied number of species for all sites for the number of individuals of the site with 
#the fewest number of individuals
hillnos$Rare<-rarefy(d,minNum)

#Estimate Chao 1 richness using a for loop - Thanks Alec!
Chao4<-t(estimateR(d[c(1),]))
for(j in 2:nrow(hillnos)){
  Chao.j<-t(estimateR(d[c(j),]))
  Chao4<-rbind(Chao4,Chao.j)
}

#Delete columns you don't need and rename as Chao1
Chao1<-Chao4[,-c(1,3,4,5)]

#Load the diverse package to calculate Berger-Parker
library(diverse)

#Calculate B-P
BP1<-as.data.frame(diversity(d,type="berger-parker"))#This gives the Berger-Parker index and its reciprocal
#Remove a column of information you don't need
hillnos$BP<-BP1[,-2] 

#Bind together the columns with the Names of the Sites, Diversity and Evenness measures, and Chao1
DivNums=cbind(Sites,hillnos,Chao1)
#Give them the names you want
colnames(DivNums)=c("Site","S", "Shannon", "Simpson", "Shannonevenness",
                    "Simpson_evenness","Fishers alpha","Number of individuals","Rarefied sp",
                    "Berger-Parker", "Chao1")
#Save your output in a table
write.table(file="Diversity and Richness Estimates Trial 1 c3.csv",sep=",",col.names=NA, DivNums)

##########################################################################################
#To make rarefaction curves
#Determine the number of individuals found in the Site with the most (maximum) individuals
maxNum<-max(rowSums(d))
#Output this number
maxNum

#Rarefy the species in each site from 1 individual to the maximum number of individuals found in the Site with the most individuals
#R will give a warning message because you can't rarefy past the number of individuals in each Site
#Don't worry about the error message because we won't be using the numbers or plotting past the number of individuals in each site
Rare1<-rarefy(d,c(1:maxNum))
#Transpose this matrix and make it a dataframe
Rare2<-as.data.frame(t(Rare1))
#Set the column names as the Sites
colnames(Rare2)<-Sites

#Make one plot with the rarefaction curves from the 4 Sites
#Each plot function here goes from 1 individual to the Number of Individuals in that site
#The data for each site is designated with the DivNums$Site[[#]] and Rare2[[#]]
#I have chosen blue, green, red, and black solid lines but you could choose different colors and line types

windows(height=5, width=7) #This just makes R pop out a window with the graph. Use "quartz" for Mac
colors<-c("gray1","gray2","gray3", "gray4", "gray5", "gray6", "gray7", "gray8", "gray9","gray10",
          "gray11","gray12","gray13", "gray14", "gray15", "gray16", "gray17", "gray18", "gray19","gray20",
          "gray21","gray22","gray23", "gray24", "gray25", "gray26", "gray27", "gray28", "gray29","gray30",
          "gray31","gray32","gray33", "gray34", "gray35", "gray36", "gray37", "gray38", "gray39","gray40",
          "gray41","gray42","gray43", "gray44", "gray45", "gray46", "gray47") #Specify a vector of colors
                          #Add more colors to this list as needed/Change as needed
SiteNames<-c(colnames(Sites)) #Specify your site names
                            #Add more Sites to this list as needed/Change names as needed
par(mar=c(5,4,1,1))#This sets how much space there should be on each side of the graph. 
                   #I don't want a lot on the top or side.

  #Loop Sites - Thanks Alec!
  for(i in 1:length(DivNums$Site)){
    plot(c(1:DivNums$`Number of individuals`[DivNums$Site[[i]]]),
         Rare2[[i]][c(1:length(c(1:DivNums$`Number of individuals`[DivNums$Site[[i]]])))],
         type='l', xlim=c(0.0,maxNum), ylim=c(0.0,25),
         xlab="Number of individuals", ylab='Number of species',col=colors[i])
    par(new=T)
  }

  legend(1,25,SiteNames,cex=1, lwd = 3, lty=1,
         col=colors,bty="n") #bty="n" means don't draw a box around the legend
par(new=F)

##You can right click on this graph once you are happy with it, copy it as a metafile, and paste it into your Word document

##############################################################################

#rank-abundance curves
#do in Excel
