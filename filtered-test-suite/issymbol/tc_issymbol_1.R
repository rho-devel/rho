expected <- eval(parse(text="FALSE"));     
test(id=0, code={     
argv <- eval(parse(text="list(0.05)"));     
do.call(`is.symbol`, argv);     
}, o=expected);     

