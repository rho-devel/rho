expected <- eval(parse(text="quote(from)"));  
test(id=0, code={  
argv <- eval(parse(text="list(function (from, strict = TRUE) from)"));  
.Internal(`body`(argv[[1]]));  
}, o=expected);  

