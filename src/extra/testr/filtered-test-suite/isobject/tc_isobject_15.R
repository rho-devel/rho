expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(TRUE, FALSE))"));             
do.call(`is.object`, argv);             
}, o=expected);             

