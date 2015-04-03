expected <- eval(parse(text="NULL"));  
test(id=0, code={  
.Internal(`sys.on.exit`());  
}, o=expected);  

