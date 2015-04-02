expected <- eval(parse(text="3L"));          
test(id=0, code={          
argv <- eval(parse(text="list(quote(cbind(X, M) ~ M.user + Temp + M.user:Temp))"));          
do.call(`length`, argv);          
}, o=expected);          

