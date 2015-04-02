expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(7L, 4L), .Dimnames = list(NULL, NULL))"));          
test(id=0, code={          
argv <- eval(parse(text="list(20, structure(c(-1, -1, -1, -1, -1, -1, -1, 8, 9, 10, 11, 12, 13, 14, 15, 100, 17, 18, 19, 20, 21, 200, 200, 24, 25, 26, 27, 28), .Dim = c(7L, 4L), .Dimnames = list(NULL, NULL)))"));          
do.call(`<`, argv);          
}, o=expected);          

