expected <- eval(parse(text="structure(list(surname = structure(c(5L, 6L, 4L, 3L, 3L, 1L, 2L), .Label = c(\"McNeil\", \"R Core\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\")), .Names = \"surname\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(surname = structure(c(5L, 6L, 4L, 3L, 3L, 1L, 2L), .Label = c(\"McNeil\", \"R Core\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"), title = structure(c(2L, 5L, 4L, 6L, 7L, 3L, 1L), .Label = c(\"An Introduction to R\", \"Exploratory Data Analysis\", \"Interactive Data Analysis\", \"LISP-STAT\", \"Modern Applied Statistics ...\", \"Spatial Statistics\", \"Stochastic Simulation\"), class = \"factor\"), other.author = structure(c(NA, 1L, NA, NA, NA, NA, 2L), .Label = c(\"Ripley\", \"Venables & Smith\"), class = \"factor\")), .Names = c(\"surname\", \"title\", \"other.author\"), row.names = c(NA, -7L), class = \"data.frame\"), 1L)"));   
do.call(`.subset`, argv);   
}, o=expected);   

