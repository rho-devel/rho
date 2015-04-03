expected <- eval(parse(text="logical(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(NULL)"));       
do.call(`is.infinite`, argv);       
}, o=expected);       

