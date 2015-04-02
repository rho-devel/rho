expected <- eval(parse(text="structure(TRUE, .Names = \"SweaveListingUtils\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(list(c(3L, 0L, 1L)), class = c(\"R_system_version\", \"package_version\", \"numeric_version\")), structure(\"2.13.2\", .Names = \"SweaveListingUtils\"))"));           
do.call(`>`, argv);           
}, o=expected);           

