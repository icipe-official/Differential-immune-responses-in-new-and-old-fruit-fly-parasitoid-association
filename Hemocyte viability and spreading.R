library(agricolae)
library(car)
library(MASS)
library(emmeans)
library(multcomp)
library(multcompView)
library(readxl)

## Viability and spreading###
#Viability====
via <- read_excel("C:/Users/Data.xlsx", sheet = 'VIABILITY and SPREADING')
names(via)
library(betareg)
via$viab<-via$Viability/100
mod_viab<-betareg(viab~Parasitoid*Fly, data = via)
summary(mod_viab)
Anova(mod_viab)
me_via<-emmeans(mod_viab,pairwise ~ Parasitoid*Fly, adjust="tukey")
cld(me_via$emmeans,alpha=.05, Letters=letters)


#Viability====
via$spread<-via$Spreading/100
mod_spread<-betareg(spread~Parasitoid*Fly, data = via)
summary(mod_spread)
Anova(mod_spread)
me_spread<-emmeans(mod_spread,pairwise ~ Parasitoid*Fly, adjust="tukey")
cld(me_spread$emmeans,alpha=.05, Letters=letters)

