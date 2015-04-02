expected <- eval(parse(text="-Inf"));              
test(id=0, code={              
argv <- eval(parse(text="list(numeric(0))"));              
do.call(`max`, argv);              
}, o=expected);              

