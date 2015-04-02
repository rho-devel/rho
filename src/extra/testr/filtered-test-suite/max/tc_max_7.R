expected <- eval(parse(text="1573.05073007216"));              
test(id=0, code={              
argv <- eval(parse(text="list(1573.05073007216, 1000)"));              
do.call(`max`, argv);              
}, o=expected);              

