expected <- eval(parse(text="\"/home/roman/r-instrumented/library\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library/graphics\")"));  
.Internal(`dirname`(argv[[1]]));  
}, o=expected);  

