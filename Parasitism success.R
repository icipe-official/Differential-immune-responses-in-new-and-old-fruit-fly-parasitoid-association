library(agricolae)
library(car)
library(MASS)
library(emmeans)
library(multcomp)
library(multcompView)
library(readxl)
library(lsmeans)
library(Rmisc)

##Parasitism success ==================
eme <- read_excel("C:/Users/Parasitism_RG.xlsx")
attach(eme)
names(eme)
e1<-glm(Proportion~Fly...2*Par, family = quasibinomial())
summary(e1)
Anova(e1)
leastsquare <-emmeans(e1, 
                      pairwise ~ Fly...2*Par,
                      adjust="tukey") 

cld(leastsquare$emmeans, 
    alpha=.05, 
    Letters=letters)
far<-summarySE(data = eme, measurevar = "percent", groupvars = c("Fly...2", "Par"))
far
