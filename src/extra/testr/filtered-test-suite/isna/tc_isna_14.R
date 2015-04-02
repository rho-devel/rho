expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c(\"High\", \"Low\"), class = \"factor\"))"));        
do.call(`is.na`, argv);        
}, o=expected);        

