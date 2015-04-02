expected <- eval(parse(text="c(FALSE, FALSE, FALSE, TRUE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(0L, 0L, 0L, 1L), .Names = c(\"Y\", \"B\", \"V\", \"N\")))"));  
do.call(`as.logical`, argv);  
}, o=expected);  

