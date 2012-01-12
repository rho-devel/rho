library(nlme)
data(petrol, package = 'MASS')
Petrol <- petrol
Petrol[, 2:5] <- scale(Petrol[, 2:5], scale = F)
pet3.lme <- lme(Y ~ SG + VP + V10 + EP,
                random = ~ 1 | No, data = Petrol, method="ML")
nform <- as.formula("Y ~ SG + VP + V10")
update(pet3.lme, nform)
