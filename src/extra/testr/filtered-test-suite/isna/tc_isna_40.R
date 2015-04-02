expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(-Inf, -Inf, -Inf, 0, 1, 2, Inf, Inf))"));        
do.call(`is.na`, argv);        
}, o=expected);        

