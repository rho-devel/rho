expected <- eval(parse(text="c(\"This is my very important data from experiment #0234\", \"Jun 5, 1998\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4, comment = c(\"This is my very important data from experiment #0234\", \"Jun 5, 1998\")))"));  
.Internal(comment(argv[[1]]));  
}, o=expected);  

