expected <- eval(parse(text="structure(c(\"exNSS4\", \"myLib\", \"1.0\", NA, \"methods\", NA, NA, NA, NA, \"GPL (>= 2)\", NA, NA, NA, NA, NA, NA, \"3.0.1\"), .Names = c(\"\", \"\", \"Version\", NA, \"Depends\", NA, NA, NA, NA, \"License\", NA, NA, NA, NA, NA, NA, \"Built\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(\"exNSS4\", \"myLib\", structure(c(\"1.0\", NA, \"methods\", NA, NA, NA, NA, \"GPL (>= 2)\", NA, NA, NA, NA, NA, NA, \"3.0.1\"), .Names = c(\"Version\", NA, \"Depends\", NA, NA, NA, NA, \"License\", NA, NA, NA, NA, NA, NA, \"Built\")))"));        
do.call(`c`, argv);        
}, o=expected);        

