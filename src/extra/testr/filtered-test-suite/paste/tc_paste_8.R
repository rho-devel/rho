expected <- eval(parse(text="\"detaching ‘package:splines’\""));                
test(id=0, code={                
argv <- eval(parse(text="list(list(\"detaching\", \"‘package:splines’\"), \" \", NULL)"));                
.Internal(paste(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

