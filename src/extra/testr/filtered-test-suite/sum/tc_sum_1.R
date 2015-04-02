expected <- eval(parse(text="313"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(313, .Names = \"\"))"));       
do.call(`sum`, argv);       
}, o=expected);       

