expected <- eval(parse(text="list(\"ret0\", c(\"Package\", \"LibPath\", \"Version\", \"Priority\", \"Depends\", \"Imports\", \"LinkingTo\", \"Suggests\", \"Enhances\", \"License\", \"License_is_FOSS\", \"License_restricts_use\", \"OS_type\", \"Archs\", \"MD5sum\", \"NeedsCompilation\", \"Built\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(\"myTst\", \"myLib\", \"1.0\", NA, \"methods\", NA, NA, NA, NA, \"What license is it under?\", NA, NA, NA, NA, NA, NA, \"3.0.1\"), .Dim = c(1L, 17L), .Dimnames = list(\"ret0\", c(\"Package\", \"LibPath\", \"Version\", \"Priority\", \"Depends\", \"Imports\", \"LinkingTo\", \"Suggests\", \"Enhances\", \"License\", \"License_is_FOSS\", \"License_restricts_use\", \"OS_type\", \"Archs\", \"MD5sum\", \"NeedsCompilation\", \"Built\"))))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

