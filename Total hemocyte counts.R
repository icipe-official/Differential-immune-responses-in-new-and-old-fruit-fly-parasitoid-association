library(agricolae)
library(car)
library(MASS)
library(emmeans)
library(multcomp)
library(multcompView)
library(readxl)
library(lsmeans)

##THC ==================
counts <- read_excel("C:/Users/Data.xlsx", sheet = 'Cells')
names(counts)

#THCs of B. dorsal##
BD_thc<-subset(counts, Fly=="BD")
mod_BDthc<-glm(round(THC,0)~Treat, family = poisson, data=BD_thc)## to make integer
summary(mod_BDthc)
Anova(mod_BDthc)
exp(coef(mod_BDthc))
exp(confint(mod_BDthc))
me_BDthc<-emmeans(mod_BDthc,pairwise ~ Treat, adjust="tukey")
cld(me_BDthc$emmeans,alpha=.05, Letters=letters)


##THCs of C. cosyra##
CC_thc<-subset(counts, Fly=="CC")
mod_CCthc<-glm(round(THC,0)~Treat, family = poisson, data=CC_thc)## to make integer
summary(mod_CCthc)
Anova(mod_CCthc)
exp(coef(mod_CCthc))
exp(confint(mod_CCthc))
me_CCthc<-emmeans(mod_CCthc,pairwise ~ Treat, adjust="tukey")
cld(me_CCthc$emmeans,alpha=.05, Letters=letters)


