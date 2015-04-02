expected <- eval(parse(text="15L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(\"1.0\", NA, NA, \"methods, graphics, pkgA\", NA, NA, NA, \"GPL (>= 2)\", NA, NA, NA, NA, NA, NA, \"R 3.0.1; ; 2014-03-17 18:49:56 UTC; unix\"), .Names = c(\"Version\", NA, NA, \"Imports\", NA, NA, NA, \"License\", NA, NA, NA, NA, NA, NA, \"Built\")))"));          
do.call(`length`, argv);          
}, o=expected);          

