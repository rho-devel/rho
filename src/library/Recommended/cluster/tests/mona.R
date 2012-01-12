library(cluster)

data(animals)
(mani <- mona(animals))

str(mani)

if(require(MASS)) {

    if(R.version$major != "1" || as.numeric(R.version$minor) >= 7)
        RNGversion("1.6")
    set.seed(253)
    n <- 512; p <- 3
    Sig <- diag(p); Sig[] <- 0.8 ^ abs(col(Sig) - row(Sig))
    x3 <- mvrnorm(n, rep(0,p), Sig) >= 0
    x <- cbind(x3, rbinom(n, size=1, prob = 1/2))

    print(sapply(as.data.frame(x), table))

    mx <- mona(x)
    str(mx)
    print(lapply(mx[c(1,3,4)], table))
}
