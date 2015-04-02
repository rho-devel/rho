expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(function (object) TRUE)"));  
.Internal(`body`(argv[[1]]));  
}, o=expected);  

