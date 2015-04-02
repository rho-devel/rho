expected <- eval(parse(text="-0.404764587672961"));       
test(id=0, code={       
argv <- eval(parse(text="list(-0.518798300715899)"));       
do.call(`expm1`, argv);       
}, o=expected);       

