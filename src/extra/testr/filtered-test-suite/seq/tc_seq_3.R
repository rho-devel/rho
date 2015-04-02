expected <- eval(parse(text="integer(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(0L)"));  
do.call(`seq_len`, argv);  
}, o=expected);  

