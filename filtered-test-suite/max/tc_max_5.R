expected <- eval(parse(text="-Inf"));              
test(id=0, code={              
argv <- list();              
do.call(`max`, argv);              
}, o=expected);              

