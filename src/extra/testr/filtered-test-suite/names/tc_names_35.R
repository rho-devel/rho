expected <- eval(parse(text="c(\"surname\", \"nationality\", \"deceased\", \"title\", \"other.author\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(surname = structure(c(\"McNeil\", \"Ripley\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"AsIs\"), nationality = structure(c(\"Australia\", \"UK\", \"UK\", \"US\", \"US\", \"Australia\"), class = \"AsIs\"), deceased = structure(c(\"no\", \"no\", \"no\", \"no\", \"yes\", \"no\"), class = \"AsIs\"), title = structure(c(\"Interactive Data Analysis\", \"Spatial Statistics\", \"Stochastic Simulation\", \"LISP-STAT\", \"Exploratory Data Analysis\", \"Modern Applied Statistics ...\"), class = \"AsIs\"), other.author = structure(c(NA, NA, NA, NA, NA, \"Ripley\"), class = \"AsIs\")), .Names = c(\"surname\", \"nationality\", \"deceased\", \"title\", \"other.author\")))"));         
do.call(`names`, argv);         
}, o=expected);         

