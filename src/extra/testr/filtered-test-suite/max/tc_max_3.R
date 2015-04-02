expected <- eval(parse(text="5"));              
test(id=0, code={              
argv <- eval(parse(text="list(5, 1, 0)"));              
do.call(`max`, argv);              
}, o=expected);              

