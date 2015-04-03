expected <- eval(parse(text="structure(integer(0), .Names = character(0))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(FALSE, .Names = \"signature-class.Rd\"))"));            
.Internal(which(argv[[1]]));            
}, o=expected);            

