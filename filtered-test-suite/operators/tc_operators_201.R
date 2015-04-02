expected <- eval(parse(text="quote(y ~ a + b:c + d + e + e:d)"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(y ~ a + b:c + d + e + e:d))"));     
do.call(`(`, argv);     
}, o=expected);     

