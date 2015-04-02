expected <- eval(parse(text="\"cnstrO> constrOptim(c(2,-1,-1), fQP, gQP, ui = t(Amat), ci = bvec)\""));                
test(id=0, code={                
argv <- eval(parse(text="list(list(\"cnstrO> \", \"constrOptim(c(2,-1,-1), fQP, gQP, ui = t(Amat), ci = bvec)\"), \"\\n\")"));                
.Internal(paste0(argv[[1]], argv[[2]]));                
}, o=expected);                

