expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(usr = c(0.568, 1.432, -1.08, 1.08), xaxp = c(0.6, 1.4, 4), yaxp = c(-1, 1, 4)), .Names = c(\"usr\", \"xaxp\", \"yaxp\")))"));        
do.call(`isS4`, argv);        
}, o=expected);        

