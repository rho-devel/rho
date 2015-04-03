expected <- eval(parse(text="\"foo-class.Rd\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"%s-class.Rd\", structure(\"foo\", .Names = \"foo\"))"));  
.Internal(`sprintf`(argv[[1]], argv[[2]]));  
}, o=expected);  

