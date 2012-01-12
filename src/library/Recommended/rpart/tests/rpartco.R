## needs to be examimed for overlaps: formerly overlapped near Type=c
pdf("rpartco-test.pdf")
library(rpart)
fit <- rpart(Price ~ Mileage + Type + Country, cu.summary)
plot(fit, compress=TRUE, branch=0)
text(fit, xpd = NA, cex = 0.7)
