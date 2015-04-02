expected <- eval(parse(text="logical(0)"));                
test(id=0, code={                
argv <- eval(parse(text="list(complex(0))"));                
do.call(`is.na`, argv);                
}, o=expected);                

