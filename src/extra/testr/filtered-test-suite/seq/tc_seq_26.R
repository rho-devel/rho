expected <- eval(parse(text="1:2"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(2, .Names = \"Ind\"))"));  
do.call(`seq_len`, argv);  
}, o=expected);  

