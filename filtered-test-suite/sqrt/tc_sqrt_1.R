expected <- eval(parse(text="3.57807204055026"));  
test(id=0, code={  
argv <- eval(parse(text="list(12.8025995273675)"));  
do.call(`sqrt`, argv);  
}, o=expected);  

