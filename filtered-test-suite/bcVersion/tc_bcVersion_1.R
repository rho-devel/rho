expected <- eval(parse(text="7L"));  
test(id=0, code={  
.Internal(`bcVersion`());  
}, o=expected);  

