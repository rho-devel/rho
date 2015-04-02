expected <- eval(parse(text="TRUE"));     
test(id=0, code={     
argv <- eval(parse(text="list(logical(0))"));     
do.call(`all`, argv);     
}, o=expected);     

