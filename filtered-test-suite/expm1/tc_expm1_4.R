expected <- eval(parse(text="numeric(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(logical(0))"));       
do.call(`expm1`, argv);       
}, o=expected);       

