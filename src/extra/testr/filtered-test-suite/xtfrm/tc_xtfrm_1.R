expected <- eval(parse(text="c(4L, 5L, 3L, 2L, 1L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(4L, 5L, 3L, 2L, 1L), .Label = c(\"McNeil\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"))"));  
do.call(`xtfrm`, argv);  
}, o=expected);  

