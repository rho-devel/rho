expected <- eval(parse(text="logical(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list())"));       
do.call(`&`, argv);       
}, o=expected);       

