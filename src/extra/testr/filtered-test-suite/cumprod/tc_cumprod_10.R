expected <- eval(parse(text="c(0.982149602642989, 0.902269182694013, 0.775382240873364, 0.71426888372223)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(0.982149602642989, 0.91866776738084, 0.859369083800704, 0.921182928974104))"));          
do.call(`cumprod`, argv);          
}, o=expected);          

