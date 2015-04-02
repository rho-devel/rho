expected <- eval(parse(text="-0.133986975078774"));  
test(id=0, code={  
argv <- eval(parse(text="list(-0.133190890463189)"));  
do.call(`atanh`, argv);  
}, o=expected);  

