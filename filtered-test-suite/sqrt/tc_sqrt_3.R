expected <- eval(parse(text="0+4.12310562561766i"));            
test(id=0, code={            
argv <- eval(parse(text="list(-17+0i)"));            
do.call(`sqrt`, argv);            
}, o=expected);            

