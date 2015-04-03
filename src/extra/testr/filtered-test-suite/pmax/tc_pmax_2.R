expected <- eval(parse(text="c(-100, 82.9775012103133, 8.55983483385341e+101, -100, 79.3831968838961, 8.55983483385341e+101)"));            
test(id=0, code={            
argv <- eval(parse(text="list(FALSE, -100, structure(c(-Inf, 82.9775012103133, 8.55983483385341e+101, -Inf, 79.3831968838961, 8.55983483385341e+101), .Names = c(\"\", \"\", \"\", \"\", \"\", \"\")))"));            
.Internal(pmax(argv[[1]], argv[[2]], argv[[3]]));            
}, o=expected);            

