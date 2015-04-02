expected <- eval(parse(text="structure(c(\"BiocInstaller\", \"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0\", \"1.12.0\", NA, \"R (>= 3.0.0)\", NA, NA, \"RUnit, BiocGenerics\", NA, \"Artistic-2.0\", NA, NA, NA, NA, NA, NA, \"3.0.1\"), .Names = c(\"\", \"\", \"Version\", NA, \"Depends\", NA, NA, \"Suggests\", NA, \"License\", NA, NA, NA, NA, NA, NA, \"Built\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(\"BiocInstaller\", \"/home/lzhao/R/x86_64-unknown-linux-gnu-library/3.0\", structure(c(\"1.12.0\", NA, \"R (>= 3.0.0)\", NA, NA, \"RUnit, BiocGenerics\", NA, \"Artistic-2.0\", NA, NA, NA, NA, NA, NA, \"3.0.1\"), .Names = c(\"Version\", NA, \"Depends\", NA, NA, \"Suggests\", NA, \"License\", NA, NA, NA, NA, NA, NA, \"Built\")))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

