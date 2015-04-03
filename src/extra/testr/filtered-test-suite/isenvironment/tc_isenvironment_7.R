expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(1+0i, 5+0i, 9+0i, 13+0i, 17+0i, 21+0i, 2+0i, 6+0i, 10+0i, 14+0i, 18+0i, 22+0i, 3+0i, 7+0i, 11+0i, 15+0i, 19+0i, 23+0i, 4+0i, 8+0i, 12+0i, 16+0i, 20+0i, 24+0i), .Dim = c(6L, 4L)))"));          
do.call(`is.environment`, argv);          
}, o=expected);          

