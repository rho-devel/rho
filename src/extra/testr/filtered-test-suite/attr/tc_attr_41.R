expected <- eval(parse(text="\"mDaniell(5)\""));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(coef = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.05), m = 5L), .Names = c(\"coef\", \"m\"), name = \"mDaniell(5)\", class = \"tskernel\"), \"name\")"));        
do.call(`attr`, argv);        
}, o=expected);        

