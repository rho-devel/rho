library(nlme)
data(Assay)
as1 <- lme(logDens~sample*dilut, data=Assay,
           random=pdBlocked(list(
                     pdIdent(~1),
                     pdIdent(~sample-1),
                     pdIdent(~dilut-1))))

update(as1,random=pdCompSymm(~sample-1))
update(as1,random=pdCompSymm(~sample-1))
update(as1,random=pdCompSymm(~sample-1))
update(as1,random=pdCompSymm(~sample-1))
