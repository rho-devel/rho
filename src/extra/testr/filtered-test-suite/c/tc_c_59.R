expected <- eval(parse(text="c(1+0i, 2+0i, 3+0i, 4+0i, 5+0i, 6+0i, 7+0i, 8+0i, 9+0i, 10+0i, 1+1i, 1+0i)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(1:10, 1+1i, TRUE)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

