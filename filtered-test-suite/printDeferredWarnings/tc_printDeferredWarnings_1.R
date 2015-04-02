expected <- eval(parse(text="NULL"));  
test(id=0, code={  
.Internal(`printDeferredWarnings`());  
}, o=expected);  

