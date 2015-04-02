expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(character(0))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

