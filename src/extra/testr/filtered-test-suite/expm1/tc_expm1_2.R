expected <- eval(parse(text="-1.5314339531682e-113"));       
test(id=0, code={       
argv <- eval(parse(text="list(-1.5314339531682e-113)"));       
do.call(`expm1`, argv);       
}, o=expected);       

