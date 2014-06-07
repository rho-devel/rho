bdeserialize()
ls()
x
xna
y
identical(y, c(0.1, pi, 0.123456789E-50, sqrt(2.0E100), Inf, -Inf, NaN, NA))
sq
z
rhubarb
is.na(rhubarb)
identical(rhubarb, c("rhubarb", NA, "rhubarb"))
custard
identical(custard, quote(custard))
my.empty.env
identical(my.empty.env, emptyenv())
my.base.env
identical(my.base.env, baseenv())
my.basenamespace
identical(my.basenamespace, .BaseNamespaceEnv)
my.global.env
identical(my.global.env, .GlobalEnv)
#my.stats.env
identical(my.stats.env, as.environment("package:stats"))
my.stats.ns
identical(my.stats.ns, environment(rnorm))
e1
ls(e1)
get("battles", envir=e2)
e2
identical(e1, e2)
