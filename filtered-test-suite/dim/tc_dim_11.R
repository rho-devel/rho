expected <- eval(parse(text="c(2L, 2L, 2L)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(c(1+1i, 3+1i, 2+1i, 4+1i, 5-1i, 7-1i, 6-1i, 8-1i), .Dim = c(2L, 2L, 2L)))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

