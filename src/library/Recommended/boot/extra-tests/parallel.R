## Reproducibility of parallel simulation
library(boot)
set.seed(123, "L'Ecuyer-CMRG")
cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
## serial version
cd4.boot <- boot(cd4, corr, R = 999, sim = "parametric",
                 ran.gen = cd4.rg, mle = cd4.mle)
boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)

for (iter in 1:2) {
    set.seed(123, "L'Ecuyer-CMRG")
    cd4.boot <- boot(cd4, corr, R = 999, sim = "parametric",
                     ran.gen = cd4.rg, mle = cd4.mle,
                     ncpus = 4, parallel = "multicore")
    print(boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
                  conf = 0.9, h = atanh, hinv = tanh))
}
for (iter in 1:2) {
    set.seed(123, "L'Ecuyer-CMRG")
    cd4.boot <- boot(cd4, corr, R = 999, sim = "parametric",
                     ran.gen = cd4.rg, mle = cd4.mle,
                     ncpus = 4, parallel = "snow")
    print(boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
                  conf = 0.9, h = atanh, hinv = tanh))
}
