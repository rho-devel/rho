expected <- eval(parse(text="structure(\"R Core\", class = \"AsIs\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(surname = structure(\"R Core\", class = \"AsIs\"), nationality = structure(NA_integer_, .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), deceased = structure(NA_integer_, .Label = c(\"no\", \"yes\"), class = \"factor\")), .Names = c(\"surname\", \"nationality\", \"deceased\"), row.names = 7L, class = \"data.frame\"), 1L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

