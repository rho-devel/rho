expected <- eval(parse(text="TRUE"));               
test(id=0, code={               
argv <- eval(parse(text="list(integer(0))"));               
do.call(`is.numeric`, argv);               
}, o=expected);               

