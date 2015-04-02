expected <- eval(parse(text="numeric(0)"));         
test(id=0, code={         
argv <- eval(parse(text="list(numeric(0))"));         
do.call(`sign`, argv);         
}, o=expected);         

