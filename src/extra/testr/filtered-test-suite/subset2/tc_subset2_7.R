expected <- eval(parse(text="structure(c(\"Tukey\", \"Venables\", \"Tierney\", \"Ripley\", \"McNeil\"), class = \"AsIs\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(surname = structure(c(\"Tukey\", \"Venables\", \"Tierney\", \"Ripley\", \"McNeil\"), class = \"AsIs\")), .Names = \"surname\"), 1L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

