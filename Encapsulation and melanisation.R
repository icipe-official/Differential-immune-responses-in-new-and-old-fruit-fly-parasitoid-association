library(agricolae)
library(car)
library(MASS)
library(emmeans)
library(multcomp)
library(multcompView)
library(readxl)

##Encapsulation and Melanisation of C. cosyra and B. dorsalis###
##ENCAPSULATION##
Enc <- read_excel("C:/Users/Data.xlsx")
View(Enc)
mod_enca<-glm(cbind(Encapsulation,Total-Encapsulation) ~ Parasitoid*Fly*Time, family = binomial, data = Enc)
summary(mod_enca)
exp(confint(mod_enca))
Anova(mod_enca, test="LR")
library(emmeans)
lsmeans = lsmeans::lsmeans
me_enc <-emmeans(mod_enca,pairwise ~ Parasitoid*Fly*Time, adjust="tukey")    
me_enc    
cld(me_enc$emmeans,alpha=.05, Letters=letters)

##Melanisation##
mod_me<-glm(cbind(Melanisation,Total-Melanisation)~ Parasitoid*Fly*Time, family = binomial, data = Enc)
summary(mod_me)
exp(coef(mod_me))
exp(confint(mod_me))
Anova(mod_me, test="LR")
lsmeans = lsmeans::lsmeans
me<-emmeans(mod_me,pairwise ~ Parasitoid*Fly*Time, adjust="tukey")
cld(me$emmeans,alpha=.05, Letters=letters)

