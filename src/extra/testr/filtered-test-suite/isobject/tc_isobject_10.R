expected <- eval(parse(text="TRUE"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(2L, .Label = c(\"McNeil\", \"R Core\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"))"));  
do.call(`is.object`, argv);  
}, o=expected);  

