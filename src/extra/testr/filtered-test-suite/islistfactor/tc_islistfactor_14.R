expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(name = structure(c(\"McNeil\", \"Ripley\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"AsIs\"), title = structure(c(\"Interactive Data Analysis\", \"Spatial Statistics\", \"Stochastic Simulation\", \"LISP-STAT\", \"Exploratory Data Analysis\", \"Modern Applied Statistics ...\"), class = \"AsIs\"), other.author = structure(c(NA, NA, NA, NA, NA, \"Ripley\"), class = \"AsIs\"), nationality = structure(c(\"Australia\", \"UK\", \"UK\", \"US\", \"US\", \"Australia\"), class = \"AsIs\"), deceased = structure(c(\"no\", \"no\", \"no\", \"no\", \"yes\", \"no\"), class = \"AsIs\")), .Names = c(\"name\", \"title\", \"other.author\", \"nationality\", \"deceased\"), row.names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\")), FALSE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

