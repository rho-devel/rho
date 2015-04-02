expected <- eval(parse(text="c(-2, 0)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(-1.94786705265839, 0.813844117537122))"));          
do.call(`floor`, argv);          
}, o=expected);          

