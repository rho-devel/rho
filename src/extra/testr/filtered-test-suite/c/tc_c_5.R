expected <- eval(parse(text="structure(-30, .Names = \"difference in location\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(`difference in location` = -30)"));        
do.call(`c`, argv);        
}, o=expected);        

