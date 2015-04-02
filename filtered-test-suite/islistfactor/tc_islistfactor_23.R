expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(surname = structure(2L, .Label = c(\"McNeil\", \"R Core\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"), nationality = structure(NA_integer_, .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), deceased = structure(NA_integer_, .Label = c(\"no\", \"yes\"), class = \"factor\")), .Names = c(\"surname\", \"nationality\", \"deceased\"), row.names = 1L, class = \"data.frame\"), structure(list(title = structure(1L, .Label = c(\"An Introduction to R\", \"Exploratory Data Analysis\", \"Interactive Data Analysis\", \"LISP-STAT\", \"Modern Applied Statistics ...\", \"Spatial Statistics\", \"Stochastic Simulation\"), class = \"factor\"), other.author = structure(2L, .Label = c(\"Ripley\", \"Venables & Smith\"), class = \"factor\")), .Names = c(\"title\", \"other.author\"), row.names = 1L, class = \"data.frame\")), FALSE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

