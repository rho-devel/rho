expected <- eval(parse(text="TRUE"));              
test(id=0, code={              
argv <- eval(parse(text="list(151.670620533678)"));              
do.call(`is.finite`, argv);              
}, o=expected);              

