expected <- eval(parse(text="\"DateTimeClasses\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"/home/roman/r-instrumented/library/base/help/DateTimeClasses\")"));  
.Internal(`basename`(argv[[1]]));  
}, o=expected);  

