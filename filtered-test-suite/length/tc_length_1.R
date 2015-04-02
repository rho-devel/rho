expected <- eval(parse(text="1L"));          
test(id=0, code={          
argv <- eval(parse(text="list(\"~ . + Soft+M.user:Temp\")"));          
do.call(`length`, argv);          
}, o=expected);          

