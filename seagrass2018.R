setwd("~/Dropbox/data/seagrass2018/")
##Mud Bay Shoots Data Sheet exploration
##Mud Bay seagrass shoots
Shoots<-read.csv("2018_MBshoots.csv", header=T)
attach(Shoots)
##initial plot of shoot density
plot(Shoots[Treatment], Shoots[Reg_shoot])
#calculate se
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]]/sqrt(length(x)), na.rm=TRUE))
  }
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
#reorder levels

#sh2 <- ordered(Shoots$Treatment, levels = c("Ambient", " + Nutrients"))
AG <- data_summary(Shoots, varname="Live_perm2", 
                    groupnames=c("Treatment", "Sampling"))
Shoots$Treatment <- factor(Shoots$Treatment, levels = c("Ambient", " + Nutrients"))
Shoots$Treatment
library(ggplot2)
# Default bar plot
MB_AG <- ggplot(sh2, aes(x=Treatment, y=Live_perm2, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Live_perm2-se, ymax=Live_perm2+se), width=.2,
                position=position_dodge(.9))

print(MB_AG)

b <- ggplot(Shoots, aes(x=Treatment, y=Live_perm2, fill=Sampling)) + 
  geom_boxplot()
print(b)

#permanova
library(vegan)

AG<-adonis(Live_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Reg_shoot_perm2 ~ Treatment*Sampling, data=Shoots)

#in summary--there was no difference in shoot density at Mud Bay after 6 weeks of treatment

##Mud Bay AGBM--let's do the same thing but for AGBM
sh2 <- data_summary(Shoots, varname="Reg_shoot_perm2", 
                    groupnames=c("Treatment", "Sampling"))

library(ggplot2)
# Default bar plot
MB_SH <- ggplot(sh2, aes(x=Treatment, y=Reg_shoot_perm2, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Reg_shoot_perm2-se, ymax=Reg_shoot_perm2+se), width=.2,
                position=position_dodge(.9))

print(MB_SH)

b <- ggplot(Shoots, aes(x=Treatment, y=Reg_shoot_perm2, fill=Sampling)) + 
  geom_boxplot()
print(b)

#permanova
library(vegan)

Sh<-adonis(Reg_shoot_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Live_perm2 ~ Treatment*Sampling, data=Shoots)

##diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots
##Shoots no difference by treatment and time

##Mud Bay AGBM--let's do the same thing but for AGBM
sh2 <- data_summary(Shoots, varname="Reg_shoot_perm2", 
                    groupnames=c("Treatment", "Sampling"))
sh2$Treatment <- factor(sh2$Treatment, levels = c("Ambient", " + Nutrients"))
sh2$Treatment

library(ggplot2)
# Default bar plot
MB_SH <- ggplot(sh2, aes(x=Treatment, y=Reg_shoot_perm2, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Reg_shoot_perm2-se, ymax=Reg_shoot_perm2+se), width=.2,
                position=position_dodge(.9))

print(MB_SH)
Shoots <- MB_SH + ggtitle("Mud Bay")+
  labs(x="", y = expression("Shoots" ~ ( m^{-2})))+
  guides(fill=guide_legend(title="Sampling")) 

shoots2 <- Shoots  + theme_grey(base_size = 15)
ggsave("MudBay.png")
dev.off()
b <- ggplot(Shoots, aes(x=Treatment, y=Reg_shoot_perm2, fill=Sampling)) + 
  geom_boxplot()
print(b)


#permanova
library(vegan)

Sh<-adonis(Reg_shoot_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Live_perm2 ~ Treatment*Sampling, data=Shoots)
# no difference between treatments and time for AGBM
##try length
l <- data_summary(Shoots, varname="Mean_L", 
                    groupnames=c("Treatment", "Sampling"))
library(ggplot2)
# Default bar plot
MB_L <- ggplot(l, aes(x=Treatment, y=Mean_L,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean_L-se, ymax=Mean_L+se), width=.2,
                position=position_dodge(.9))

print(MB_L)

l <- ggplot(Shoots, aes(x=Treatment, y=Mean_L, fill=Sampling)) + 
  geom_boxplot()
print(l)

#permanova
library(vegan)

LL<-adonis(Mean_L~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Mean_L ~ Treatment*Sampling, data=Shoots)

##diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
##no difference in length by treatment and sampling

##look at detrituc

d <- data_summary(Shoots, varname="Detritus_perm2", 
                  groupnames=c("Treatment", "Sampling"))
library(ggplot2)
# Default bar plot
MB_D <- ggplot(d, aes(x=Treatment, y=Detritus_perm2,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Detritus_perm2-se, ymax=Detritus_perm2+se), width=.2,
                position=position_dodge(.9))

print(MB_D)

D <- ggplot(Shoots, aes(x=Treatment, y=Detritus_perm2, fill=Sampling)) + 
  geom_boxplot()
print(D)
Detritus <- D + ggtitle("Mud Bay")+
  labs(x="", y = expression("Detritus" ~ (g ~ m^{-2})))+
  guides(fill=guide_legend(title="Sampling"))
  
Detritus
Detritus2 <- Detritus + theme_grey(base_size = 15)
Detritus2
ggsave("MBdetritus.png")
#permanova
library(vegan)

DD<-adonis(Detritus_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Detritus_perm2 ~ Treatment*Sampling, data=Shoots)

###Mud Bay algae
##Epiphytes
Epi<-read.csv("2018_MBepi.csv", header=T)
attach(Epi)

#calculate se
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]]/sqrt(length(x)), na.rm=TRUE))
  }
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
#reorder levels

#sh2 <- ordered(Shoots$Treatment, levels = c("Ambient", " + Nutrients"))
EBM <- data_summary(Epi, varname="Net_epiwt", 
                   groupnames=c("Treatment", "Sampling"))
Epi$Treatment <- factor(Epi$Treatment, levels = c("Ambient", " + Nutrients"))
Epi$Treatment
library(ggplot2)
# Default bar plot
MB_Epi <- ggplot(EBM, aes(x=Treatment, y=Net_epiwt, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Net_epiwt-se, ymax=Net_epiwt+se), width=.2,
                position=position_dodge(.9))

print(MB_Epi)

e <- ggplot(Epi, aes(x=Treatment, y=Ratio, fill=Sampling)) + 
  geom_boxplot()
print(e)
###try ratio as well
EBM <- data_summary(Epi, varname="Ratio", 
                    groupnames=c("Treatment", "Sampling"))
Epi$Treatment <- factor(Epi$Treatment, levels = c("Ambient", " + Nutrients"))
Epi$Treatment
library(ggplot2)
# Default bar plot
MB_Epi <- ggplot(EBM, aes(x=Treatment, y=Net_epiwt, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Net_Ratio-se, ymax=Net_epiwt+se), width=.2,
                position=position_dodge(.9))

print(MB_Epi)
##Ratio box plot
r <- ggplot(Epi, aes(x=Treatment, y=Ratio, fill=Sampling)) + 
  geom_boxplot()
print(r)

##Net biomass plot
BM <- ggplot(Epi, aes(x=Treatment, y=Net_biomasswt, fill=Sampling)) + 
  geom_boxplot()
print(BM)
B <-adonis(Net_biomasswt~Treatment*Sampling,data=Epi,permutations = 9999,method = "euclidean")
fitB <- aov(Net_biomasswt ~ Treatment*Sampling, data=Epi)
#permanova 
library(vegan)
##net epi
EP<-adonis(Net_epiwt~Treatment*Sampling,data=Epi,permutations = 9999,method = "euclidean")
fit <- aov(Reg_shoot_perm2 ~ Treatment*Sampling, data=Epi)
##Ratio epi/seagrass
R<-adonis(Ratio~Treatment*Sampling,data=Epi,permutations = 9999,method = "euclidean")
fit <- aov(Reg_shoot_perm2 ~ Treatment*Sampling, data=Epi)


##diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots


###Macroalgae

Macro<-read.csv("2018_MBmacro.csv", header=T)
attach(Macro)


M <- ggplot(Macro, aes(x=Treatment, y=Macro_perm2, fill=Sampling)) + 
  geom_boxplot()
print(M)
##number of species

MBMacNum<-read.csv("2018_MBMAnum.csv", header=T)
attach(MBMacNum)
##cslc SE
MBMacNum$Treatment <- factor(MBMacNum$Treatment, levels = c("Ambient", " +Nutrients"))
MBMacNum$Treatment
MBMacNum$Sampling <- factor(MBMacNum$Sampling, levels = c("Initial ", "6Week"))
MBMacNum$Sampling
m <- data_summary(MBMacNum, varname="Num_Species", 
                  groupnames=c("Treatment", "Sampling"))
weight <-data_summary(MBMacNum, varname="Sum_weight", 
                      groupnames=c("Treatment", "Sampling"))
library(ggplot2)

# Default bar plot, number macro
MB_MN <- ggplot(m, aes(x=Treatment, y=Num_Species,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Num_Species-se, ymax=Num_Species+se), width=.2,
                position=position_dodge(.9))

print(MB_MN)
MN <- MB_MN + ggtitle("Mud Bay")+
  labs(x="", y = "Number of Macroalgal Species")+
  guides(fill=guide_legend(title="Sampling"))

MN
MN2 <- MN + theme_grey(base_size = 15)
MN2
ggsave("MBMacroNum.png")
##################
####now same thing for Crescent Beach
Shoots<-read.csv("2018_CBshoots.csv", header=T)
attach(Shoots)

#calculate se
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]]/sqrt(length(x)), na.rm=TRUE))
  }
  
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
#reorder levels

#sh2 <- ordered(Shoots$Treatment, levels = c("Ambient", " + Nutrients"))
AG <- data_summary(Shoots, varname="Live_perm2", 
                   groupnames=c("Treatment", "Sampling"))
Shoots$Treatment <- factor(Shoots$Treatment, levels = c("Ambient", " + Nutrients"))
Shoots$Treatment
sh2 <- data_summary(Shoots, varname="Reg_shoot_perm2", 
                    groupnames=c("Treatment", "Sampling"))

library(ggplot2)
# Default bar plot
CB_SH <- ggplot(sh2, aes(x=Treatment, y=Reg_shoot_perm2, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Reg_shoot_perm2-se, ymax=Reg_shoot_perm2+se), width=.2,
                position=position_dodge(.9))

print(CB_SH)
Shoots <- CB_SH + ggtitle("Crescent Beach")+
  labs(x="", y = expression("Shoots" ~ ( m^{-2})))+
  guides(fill=guide_legend(title="Sampling")) 

shoots2 <- Shoots  + theme_grey(base_size = 15)
ggsave("CrescentBeach.png")
b <- ggplot(Shoots, aes(x=Treatment, y=Reg_shoot_perm2, fill=Sampling)) + 
  geom_boxplot()
print(b)

#permanova
library(vegan)

Sh<-adonis(Reg_shoot_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Live_perm2 ~ Treatment*Sampling, data=Shoots)

#permanova
library(vegan)

AG<-adonis(Live_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Reg_shoot_perm2 ~ Treatment*Sampling, data=Shoots)

#in summary--there was no difference in shoot density at Mud Bay after 6 weeks of treatment

##Crescent Beach AGBM--let's do the same thing but for AGBM
AG2 <- data_summary(Shoots, varname="Live_perm2", 
                    groupnames=c("Treatment", "Sampling"))

library(ggplot2)
# Default bar plot
CB_AG <- ggplot(AG2, aes(x=Treatment, y=Live_perm2, fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Live_perm2-se, ymax=Live_perm2+se), width=.2,
                position=position_dodge(.9))

print(CB_AG)

ag <- ggplot(Shoots, aes(x=Treatment, y=Live_perm2, fill=Sampling)) + 
  geom_boxplot()
print(ag)

#permanova
library(vegan)

Ag<-adonis(Live_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Live_perm2 ~ Treatment*Sampling, data=Shoots)

##diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

##AGBM difference by sampling but not treatment--consider a calculation of "change"?

##try length
l <- data_summary(Shoots, varname="Mean_L", 
                  groupnames=c("Treatment", "Sampling"))
library(ggplot2)
# Default bar plot
CB_L <- ggplot(l, aes(x=Treatment, y=Mean_L,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean_L-se, ymax=Mean_L+se), width=.2,
                position=position_dodge(.9))

print(CB_L)

l <- ggplot(Shoots, aes(x=Treatment, y=Mean_L, fill=Sampling)) + 
  geom_boxplot()
print(l)

#permanova
library(vegan)

LL<-adonis(Mean_L~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Mean_L ~ Treatment*Sampling, data=Shoots)

##diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit)
##no difference in length by treatment and sampling

##look at detrituc

d <- data_summary(Shoots, varname="Detritus_perm2", 
                  groupnames=c("Treatment", "Sampling"))
library(ggplot2)
# Default bar plot
CB_D <- ggplot(d, aes(x=Treatment, y=Detritus_perm2,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Detritus_perm2-se, ymax=Detritus_perm2+se), width=.2,
                position=position_dodge(.9))

print(CB_D)

D <- ggplot(Shoots, aes(x=Treatment, y=Detritus_perm2, fill=Sampling)) + 
  geom_boxplot()
print(D)
Detritus <- D + ggtitle("Cresecent Beach")+
  labs(x="", y = expression("Detritus" ~ (g ~ m^{-2})))+
  guides(fill=guide_legend(title="Sampling"))

Detritus
Detritus2 <- Detritus + theme_grey(base_size = 15)
Detritus2
ggsave("CBDetritus.png")
#permanova
library(vegan)

DD<-adonis(Detritus_perm2~Treatment*Sampling,data=Shoots,permutations = 9999,method = "euclidean")
fit <- aov(Detritus_perm2 ~ Treatment*Sampling, data=Shoots)
summary(fit)

###Crescent Beach algae
##Epiphytes
EpiCB<-read.csv("2018_CBepi.csv", header=T)
attach(EpiCB)
EpiCB$Treatment <- factor(EpiCB$Treatment, levels = c("Ambient", " + Nutrients"))
Epi$Treatment
EpiCB$Sampling <- factor(EpiCB$Sampling, levels = c("Initial", "Week4"))
EpiCB$Sampling
library(ggplot2)

nCB <- ggplot(EpiCB, aes(x=Treatment, y=Net_epiwt, fill=Sampling)) + 
geom_boxplot()
print(nCB)
N <-adonis(Net_epiwt~Treatment*Sampling,data=EpiCB,permutations = 9999,method = "euclidean")

##Ratio box plot
rCB <- ggplot(EpiCB, aes(x=Treatment, y=Ratio, fill=Sampling)) + 
  geom_boxplot()
print(rCB)

##Net biomass plot
BMCB <- ggplot(EpiCB, aes(x=Treatment, y=Net_biomasswt, fill=Sampling)) + 
  geom_boxplot()
print(BMCB)
B <-adonis(Net_biomasswt~Treatment*Sampling,data=Epi,permutations = 9999,method = "euclidean")
fitB <- aov(Net_biomasswt ~ Treatment*Sampling, data=Epi)

##CB Macroalgae
CBMacro<-read.csv("2018_CBmacro2.csv", header=T)
attach(CBMacro)


MC <- ggplot(CBMacro, aes(x=Treatment, y=Macro_perm2, fill=Sampling)) + 
  geom_boxplot()
print(MC)

CBMacro<-read.csv("2018_CBmacro2.csv", header=T)
attach(CBMacro)

##number of species

CBMacNum<-read.csv("2018_CBMAnum.csv", header=T)
attach(CBMacNum)
##cslc SE
CBMacNum$Treatment <- factor(CBMacNum$Treatment, levels = c("Ambient", " +Nutrients"))
CBMacNum$Treatment
CBMacNum$Sampling <- factor(CBMacNum$Sampling, levels = c("Initial ", "1Month"))
CBMacNum$Sampling
m <- data_summary(CBMacNum, varname="Num_Species", 
                  groupnames=c("Treatment", "Sampling"))
weight <-data_summary(CBMacNum, varname="Sum_weight", 
                      groupnames=c("Treatment", "Sampling"))
library(ggplot2)

# Default bar plot, number macro
CB_MN <- ggplot(m, aes(x=Treatment, y=Num_Species,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Num_Species-se, ymax=Num_Species+se), width=.2,
                position=position_dodge(.9))

print(CB_MN)
MN <- CB_MN + ggtitle("Cresecent Beach")+
  labs(x="", y = "Number of Macroalgal Species")+
  guides(fill=guide_legend(title="Sampling"))

MN
MN2 <- MN + theme_grey(base_size = 15)
MN2
ggsave("CBMacroNum.png")


#sum weight bar
CB_sum <- ggplot(weight, aes(x=Treatment, y=Sum_weight,  fill=Sampling)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Sum_weight-se, ymax=Sum_weight+se), width=.2,
                position=position_dodge(.9))

print(CB_sum)
##box plot
Num <- ggplot(CBMacNum, aes(x=Treatment, y=Num_Species, fill=Sampling)) + 
  geom_boxplot()
print(Num)
Detritus <- D + ggtitle("Mud Bay")+
  labs(x="", y = expression("Detritus" ~ (g ~ m^{-2})))+
  guides(fill=guide_legend(title="Sampling"))