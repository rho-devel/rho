expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(FALSE)"));             
do.call(`nzchar`, argv);             
}, o=expected);             

