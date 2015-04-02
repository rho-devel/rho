expected <- eval(parse(text="c(11354, 11382, 11413)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(11354, 11382, 11413), class = \"Date\"))"));  
do.call(`xtfrm`, argv);  
}, o=expected);  

