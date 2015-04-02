expected <- eval(parse(text="0.707106781186548+0.707106781186548i"));            
test(id=0, code={            
argv <- eval(parse(text="list(0+1i)"));            
do.call(`sqrt`, argv);            
}, o=expected);            

