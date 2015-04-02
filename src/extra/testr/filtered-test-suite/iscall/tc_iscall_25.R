expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(cbind(X, M) ~ M.user + Temp + M.user:Temp))"));       
do.call(`is.call`, argv);       
}, o=expected);       

