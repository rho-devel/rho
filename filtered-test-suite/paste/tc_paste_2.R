expected <- eval(parse(text="\"[\\rcox.zph\\rNA\""));                
test(id=0, code={                
argv <- eval(parse(text="list(list(c(\"[\", \"cox.zph\", NA)), \" \", \"\\r\")"));                
.Internal(paste(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

