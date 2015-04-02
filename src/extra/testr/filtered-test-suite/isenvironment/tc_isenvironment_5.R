expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(5L, 5L)))"));          
do.call(`is.environment`, argv);          
}, o=expected);          

