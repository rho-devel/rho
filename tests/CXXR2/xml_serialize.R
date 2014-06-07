x <- 1:10
xna <- c(NA, 2, NA, NA, 5, 6, NA, 8, 9, NA)
y <- c(0.1, pi, 0.123456789E-50, sqrt(2.0E100), Inf, -Inf, NaN, NA)
sq <- function(y) {y*y}
z <- sq(x)
rhubarb <- c("rhubarb", NA, "rhubarb")
custard <- quote(custard)
my.empty.env <- emptyenv()
my.base.env <- baseenv()
my.basenamespace <- .BaseNamespaceEnv
my.global.env <- .GlobalEnv
my.stats.env <- as.environment("package:stats")
my.stats.ns <- environment(rnorm)
e1 <- new.env()
assign("battles", c(1066, 1485), e1)
e2 <- e1
bserialize()
