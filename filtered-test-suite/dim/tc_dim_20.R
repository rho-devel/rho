expected <- eval(parse(text="c(0L, 0L)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(raw(0), .Dim = c(0L, 0L)))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

