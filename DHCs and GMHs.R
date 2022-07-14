library(agricolae)
library(car)
library(MASS)
library(emmeans)
library(multcomp)
library(multcompView)
library(readxl)
library(lsmeans)
library(betareg)

##DHCs ==================
counts <- read_excel("C:/Users/Data.xlsx", sheet = 'Cells')
names(counts)

#DHCs of B. dorsal##
BD<-subset(counts, Fly=="BD") ##subset BD fo DHCs

#Plasmatocytes
BD$plasma<-BD$Plasmatocytes/100
mod_plas<-betareg(plasma~Treat, data = BD)
summary(mod_plas)
Anova(mod_plas)
marginal = emmeans(mod_plas,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Granulocytes
BD$granu<-BD$Granulocytes/100
mod_gran<-betareg(granu~Treat, data = BD)
summary(mod_gran)
Anova(mod_gran)
marginal = emmeans(mod_gran,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Prohemocytes
BD$pro<-BD$Prohemocytes/100
mod_pro<-betareg(pro~Treat, data = BD)
summary(mod_pro)
Anova(mod_pro)
marginal = emmeans(mod_pro,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Oenocytes
BD$oen<-BD$Oenocytes/100
mod_oen<-betareg(oen~Treat, data = BD)
summary(mod_oen)
Anova(mod_oen)
marginal = emmeans(mod_oen,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Spherulocytes
BD$sph<-BD$Spherulocytes/100
mod_sph<-betareg(sph~Treat, data = BD)
summary(mod_sph)
Anova(mod_sph)
marginal = emmeans(mod_sph,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#adipohemocytes
BD$adip<-BD$Adipocytes/100
mod_adip<-betareg(granu~Treat, data = BD)
summary(mod_adip)
Anova(mod_adip)
marginal = emmeans(mod_adip,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

##DHCs of C. cosyra##
CC<-subset(counts, Fly=="CC") ##subset CC for DHCs

#Plasmatocytes
CC$plasma<-CC$Plasmatocytes/100
mod_plas<-betareg(plasma~Treat, data = CC)
summary(mod_plas)
Anova(mod_plas)
marginal = emmeans(mod_plas,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Granulocytes
CC$granu<-CC$Granulocytes/100
mod_gran<-betareg(granu~Treat, data = CC)
summary(mod_gran)
Anova(mod_gran)
marginal = emmeans(mod_gran,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Prohemocytes
CC$pro<-CC$Prohemocytes/100
mod_pro<-betareg(pro~Treat, data = CC)
summary(mod_pro)
Anova(mod_pro)
marginal = emmeans(mod_pro,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Oenocytes
CC$oen<-CC$Oenocytes/100
mod_oen<-betareg(oen~Treat, data = CC)
summary(mod_oen)
Anova(mod_oen)
marginal = emmeans(mod_oen,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

#Spherulocytes
CC$sph<-CC$Spherulocytes/100
mod_sph<-betareg(sph~Treat, data = CC)
summary(mod_sph)
Anova(mod_sph)
marginal = emmeans(mod_sph,
                   ~ Treat)
pairs(marginal,
      adjust="tukey")
Sum = cld(marginal,
          alpha   = 0.05,
          Letters = letters,      
          adjust  = "tukey")         
Sum

##GMHs in B. dorsalis=====
GMH <- read_excel("C:/Users/Daisy/Data.xlsx", sheet = 'GMH')
names(GMH)
mod_gmh<-glm(round(GMH)~Parasitoid*Time, family = poisson, data=GMH)## to make integer
summary(mod_gmh)
Anova(mod_gmh)
