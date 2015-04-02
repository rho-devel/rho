expected <- eval(parse(text="\"unknown\""));                
test(id=0, code={                
argv <- eval(parse(text="list(\"detaching ‘package:nlme’, ‘package:splines’\")"));                
.Internal(Encoding(argv[[1]]));                
}, o=expected);                

