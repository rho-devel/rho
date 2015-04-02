expected <- eval(parse(text="0.25069599964819"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(0.25069599964819, 0.252830784944624), .Dim = 1:2))"));             
do.call(`min`, argv);             
}, o=expected);             

