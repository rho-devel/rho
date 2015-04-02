expected <- eval(parse(text="Inf"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"Inf\")"));     
do.call(`as.double`, argv);     
}, o=expected);     

