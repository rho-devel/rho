x <- 1:10
sq <- function(y) {y*y}
z <- sq(x)
rhubarb <- c("rhubarb", NA, "rhubarb")
custard <- quote(custard)
my.empty.env <- emptyenv()
my.base.env <- baseenv()
my.basenamespace <- .BaseNamespaceEnv
my.global.env <- .GlobalEnv
e1 <- new.env()
assign("battles", c(1066, 1485), e1)
e2 <- e1
bserialize()
