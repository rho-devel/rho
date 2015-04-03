expected <- eval(parse(text="0L"));          
test(id=0, code={          
argv <- eval(parse(text="list(complex(0))"));          
do.call(`length`, argv);          
}, o=expected);          

