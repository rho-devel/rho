expected <- eval(parse(text="-1.46941282670977e-16"));     
test(id=0, code={     
argv <- eval(parse(text="list(-1.46941282670977e-16)"));     
do.call(`atan`, argv);     
}, o=expected);     

