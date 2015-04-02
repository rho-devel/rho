expected <- eval(parse(text="c(\"a..ef\", \"q+++ty\", \"y..op[\", \"b\", \"s..ff.blah.yech\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"asfef\", \"qwerty\", \"yuiop[\", \"b\", \"stuff.blah.yech\"), 2L, 1000000L, c(\"..\", \"+++\"))"));  
.Internal('substr<-'(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

