expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(13186.6170826564)"));       
do.call(`is.list`, argv);       
}, o=expected);       

