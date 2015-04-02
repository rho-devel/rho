expected <- eval(parse(text="c(FALSE, FALSE)"));        
test(id=0, code={        
argv <- eval(parse(text="list(119:120)"));        
do.call(`is.na`, argv);        
}, o=expected);        

