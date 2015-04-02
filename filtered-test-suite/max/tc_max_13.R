expected <- eval(parse(text="6"));              
test(id=0, code={              
argv <- eval(parse(text="list(6L, numeric(0))"));              
do.call(`max`, argv);              
}, o=expected);              

