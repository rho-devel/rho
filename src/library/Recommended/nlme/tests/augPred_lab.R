library(nlme)
if(require("Hmisc")) {
    T.aug <- Orthodont
    label(T.aug$age) <- 'anyL'
    foo <- augPred(lme(distance ~ age, random = ~1|Subject, data=T.aug))
    ## failed in 3.1-72
}

## failed even if there is a variable with a class that is not being used.
T.aug <- Orthodont
T.aug$newage <- T.aug$age
class(T.aug$newage) <- 'anyC'
foo <- augPred(lme(distance ~ age, random = ~1|Subject, data=T.aug))
## failed in 3.1-72
