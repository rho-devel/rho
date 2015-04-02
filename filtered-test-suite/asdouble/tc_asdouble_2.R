expected <- eval(parse(text="c(10, 2.7404, 0.27404, NA)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"10\", \"2.7404\", \"0.27404\", \"\"))"));     
do.call(`as.double`, argv);     
}, o=expected);     

