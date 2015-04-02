expected <- eval(parse(text="\"/home/lzhao/hg/r-instrumented/tests/Packages/survival/inst\""));        
test(id=0, code={        
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/tests/Packages/survival/inst/CITATION\")"));        
.Internal(dirname(argv[[1]]));        
}, o=expected);        

