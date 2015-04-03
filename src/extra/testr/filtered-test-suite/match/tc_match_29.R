expected <- eval(parse(text="c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"TRUE\", \"FALSE\", \"TRUE\", \"FALSE\", \"TRUE\", \"FALSE\", \"TRUE\", \"FALSE\", \"TRUE\", \"FALSE\"), c(FALSE, TRUE), NA_integer_, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

