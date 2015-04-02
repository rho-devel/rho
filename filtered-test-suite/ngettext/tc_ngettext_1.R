expected <- eval(parse(text="\"%s is not TRUE\""));  
test(id=0, code={  
argv <- eval(parse(text="list(1L, \"%s is not TRUE\", \"%s are not all TRUE\", NULL)"));  
.Internal(`ngettext`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

