expected <- eval(parse(text="5L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(name = structure(c(\"McNeil\", \"Ripley\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"AsIs\"), title = structure(c(3L, 6L, 7L, 4L, 2L, 5L), .Label = c(\"An Introduction to R\", \"Exploratory Data Analysis\", \"Interactive Data Analysis\", \"LISP-STAT\", \"Modern Applied Statistics ...\", \"Spatial Statistics\", \"Stochastic Simulation\"), class = \"factor\"), other.author = structure(c(NA, NA, NA, NA, NA, 1L), .Label = c(\"Ripley\", \"Venables & Smith\"), class = \"factor\"), nationality = structure(c(1L, 2L, 2L, 3L, 3L, 1L), .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), deceased = structure(c(1L, 1L, 1L, 1L, 2L, 1L), .Label = c(\"no\", \"yes\"), class = \"factor\")), .Names = c(\"name\", \"title\", \"other.author\", \"nationality\", \"deceased\"), row.names = c(6L, 4L, 5L, 3L, 1L, 2L), class = \"data.frame\"))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

