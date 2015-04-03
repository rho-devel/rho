expected <- eval(parse(text="c(\"FALSE\", \"More testing :\", \"12321\", \"B2\")"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(FALSE, \"More testing :\", 12321, \"B2\")"));                  
do.call(`c`, argv);                  
}, o=expected);                  

