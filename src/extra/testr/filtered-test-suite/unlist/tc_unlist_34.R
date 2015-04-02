expected <- eval(parse(text="c(\"McNeil\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\", \"Australia\", \"UK\", \"US\", \"US\", \"Australia\", \"no\", \"no\", \"no\", \"yes\", \"no\", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(surname = structure(c(\"McNeil\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"AsIs\"), nationality = structure(c(\"Australia\", \"UK\", \"US\", \"US\", \"Australia\"), class = \"AsIs\"), deceased = structure(c(\"no\", \"no\", \"no\", \"yes\", \"no\"), class = \"AsIs\"), title = structure(c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), class = \"AsIs\"), other.author = structure(c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_), class = \"AsIs\")), .Names = c(\"surname\", \"nationality\", \"deceased\", \"title\", \"other.author\"), row.names = c(\"1\", \"2\", \"3\", \"4\", \"5\")), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

