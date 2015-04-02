expected <- eval(parse(text="c(\"Australia\", \"UK\", \"UK\", \"US\", \"US\", \"Australia\", NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1L, 2L, 2L, 3L, 3L, 1L, NA), .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"))"));        
do.call(`as.character`, argv);        
}, o=expected);        

