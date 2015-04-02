expected <- eval(parse(text="structure(7.17940517593473, .Names = \"x\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(3, structure(2.39313505864491, .Names = \"x\"))"));                
do.call(`*`, argv);                
}, o=expected);                

