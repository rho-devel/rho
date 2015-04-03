expected <- eval(parse(text="0+1i"));  
test(id=0, code={  
argv <- eval(parse(text="list(1.54308063481524+0i)"));  
do.call(`acos`, argv);  
}, o=expected);  

