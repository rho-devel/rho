expected <- eval(parse(text="\"  ‘help.search()’ or ‘??\""));                
test(id=0, code={                
argv <- eval(parse(text="list(list(\"  ‘help.search()’ or ‘\", \"??\"), NULL)"));                
.Internal(paste0(argv[[1]], argv[[2]]));                
}, o=expected);                

