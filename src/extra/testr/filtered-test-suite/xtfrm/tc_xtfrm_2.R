expected <- eval(parse(text="c(6L, 7L, 5L, 3L, 3L, 1L, 2L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"Tukey\", \"Venables\", \"Tierney\", \"Ripley\", \"Ripley\", \"McNeil\", \"R Core\"), class = \"AsIs\"))"));     
do.call(`xtfrm`, argv);     
}, o=expected);     

