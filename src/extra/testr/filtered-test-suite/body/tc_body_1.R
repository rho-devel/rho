expected <- eval(parse(text="quote({    c(x, y)})"));  
test(id=0, code={  
argv <- eval(parse(text="list(function (x, y) {    c(x, y)})"));  
.Internal(`body`(argv[[1]]));  
}, o=expected);  

