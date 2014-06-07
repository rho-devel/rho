## Example of scoping problem.
## Originally from a report by Markus Jantti:
## https://stat.ethz.ch/pipermail/r-help/2005-November/081382.html
library(nlme)
## stolen from example(anova.gls)
# AR(1) errors within each Mare
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
# variance changes with a power of the absolute fitted values?
fm2 <- update(fm1, weights = varPower())
anova(fm1, fm2)

## now define a little function
dummy <- function(obj) anova(obj[[1]], obj[[2]])
dummy(list(fm1, fm2))
## last failed < 3.1-66

## PR#13567
fm1Orth.gls <- gls(distance ~ Sex * I(age - 11), Orthodont,
                   correlation = corSymm(form = ~ 1 | Subject),
                   weights = varIdent(form = ~ 1 | age))
anova(fm1Orth.gls, Terms = "Sex")
