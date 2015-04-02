expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(\"(2,5.5]\", \"(5.5,10]\", NA))"));          
do.call(`is.null`, argv);          
}, o=expected);          

