expected <- eval(parse(text="numeric(0)"));             
test(id=0, code={             
argv <- eval(parse(text="list(character(0))"));             
do.call(`cumsum`, argv);             
}, o=expected);             

