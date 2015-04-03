expected <- eval(parse(text="1.3e+100"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(1.2e+100, 1.3e+100))"));              
do.call(`max`, argv);              
}, o=expected);              

