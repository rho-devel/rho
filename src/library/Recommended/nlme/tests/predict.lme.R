## from PR#8905
library(nlme)
fm <- lme(distance ~ poly(age, 3) + Sex, data = Orthodont, random = ~ 1)
# data for predictions
Newdata <- head(Orthodont)
Newdata$Sex <- factor(Newdata$Sex, levels = levels(Orthodont$Sex))
(pr <- predict(fm, Newdata))
stopifnot(all.equal(c(pr), fitted(fm)[1:6]))
