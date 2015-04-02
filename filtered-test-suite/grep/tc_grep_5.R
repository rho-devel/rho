expected <- eval(parse(text="12L"));     
test(id=0, code={     
argv <- eval(parse(text="list(\".__T__[:\", c(\".__T__plot:graphics\", \".__T__$:base\", \".__T__$<-:base\", \".__T__Arith:base\", \".__T__Compare:methods\", \".__T__Complex:base\", \".__T__Logic:base\", \".__T__Math2:methods\", \".__T__Math:base\", \".__T__Ops:base\", \".__T__Summary:base\", \".__T__[:base\", \".__T__addNextMethod:methods\", \".__T__body<-:base\", \".__T__cbind2:methods\", \".__T__coerce:methods\", \".__T__coerce<-:methods\", \".__T__initialize:methods\", \".__T__kronecker:base\", \".__T__loadMethod:methods\", \".__T__rbind2:methods\", \".__T__show:methods\", \".__T__slotsFromS3:methods\"), FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)"));     
.Internal(`grep`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));     
}, o=expected);     

