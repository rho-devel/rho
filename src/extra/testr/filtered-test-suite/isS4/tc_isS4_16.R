expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(4.94065645841247e-324)"));          
do.call(`isS4`, argv);          
}, o=expected);          

