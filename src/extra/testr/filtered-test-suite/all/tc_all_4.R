expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(0, 0, 3, 0, 0, 0, 1, 0, 0, 2, 2, 3.2, -1, 1, 3.2, 4, 3, 0, 0, 0, 0, 3.2, 0, 0, 3.2, 0, 202, 0, 0, 0, 241, 0, 243, 0, 0), .Dim = c(5L, 7L), .Dimnames = list(c(\"r1\", \"r2\", \"r3\", \"r4\", \"r5\"), c(\"c1\", \"c2\", \"c3\", \"c4\", \"c5\", \"c6\", \"c7\"))))"));            
do.call(`all`, argv);            
}, o=expected);            

