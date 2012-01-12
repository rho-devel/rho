### regression tests

library(MASS)

## uses Matrix
contr.sdif(6)
contr.sdif(6, sparse=TRUE)
stopifnot(all(contr.sdif(6) == contr.sdif(6, sparse=TRUE)))

