expected <- eval(parse(text="list(function (x, i, j, ...) x@aa[[i]])"));         
test(id=0, code={         
argv <- eval(parse(text="list(function (x, i, j, ...) x@aa[[i]])"));         
do.call(`list`, argv);         
}, o=expected);         

