expected <- eval(parse(text="structure(list(surname = structure(2L, .Label = c(\"McNeil\", \"R Core\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"), nationality = structure(NA_integer_, .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), deceased = structure(NA_integer_, .Label = c(\"no\", \"yes\"), class = \"factor\")), .Names = c(\"surname\", \"nationality\", \"deceased\"), row.names = 7L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(surname = structure(2L, .Label = c(\"McNeil\", \"R Core\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"), nationality = structure(NA_integer_, .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), deceased = structure(NA_integer_, .Label = c(\"no\", \"yes\"), class = \"factor\")), .Names = c(\"surname\", \"nationality\", \"deceased\"), row.names = 7L))"));     
do.call(`unclass`, argv);     
}, o=expected);     

