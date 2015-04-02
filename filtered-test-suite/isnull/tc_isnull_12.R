expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(1.74126257032961e-18)"));          
do.call(`is.null`, argv);          
}, o=expected);          

