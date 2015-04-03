expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(FALSE, TRUE, FALSE))"));             
do.call(`any`, argv);             
}, o=expected);             

