expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(raw(0))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

