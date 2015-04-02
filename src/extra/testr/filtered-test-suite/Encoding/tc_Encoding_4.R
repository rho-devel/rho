expected <- eval(parse(text="character(0)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(character(0), class = \"check_code_usage_in_package\"))"));                
.Internal(Encoding(argv[[1]]));                
}, o=expected);                

