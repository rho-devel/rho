expected <- eval(parse(text="\"Report Information on C Stack Size and Usage\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"Report Information on C Stack Size and Usage\", \"UTF-8\", \"\", \"byte\", FALSE, FALSE)"));  
.Internal(`iconv`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));  
}, o=expected);  

