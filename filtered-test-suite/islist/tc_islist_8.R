expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(title = structure(1L, .Label = c(\"An Introduction to R\", \"Exploratory Data Analysis\", \"Interactive Data Analysis\", \"LISP-STAT\", \"Modern Applied Statistics ...\", \"Spatial Statistics\", \"Stochastic Simulation\"), class = \"factor\"), other.author = structure(2L, .Label = c(\"Ripley\", \"Venables & Smith\"), class = \"factor\")), .Names = c(\"title\", \"other.author\"), row.names = 1L, class = \"data.frame\"))"));       
do.call(`is.list`, argv);       
}, o=expected);       

