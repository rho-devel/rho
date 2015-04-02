expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(NA, \"Ripley\", \"Venables & Smith\"))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

