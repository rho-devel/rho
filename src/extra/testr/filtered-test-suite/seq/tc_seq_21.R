expected <- eval(parse(text="1:3"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(surname = structure(integer(0), .Label = c(\"McNeil\", \"Ripley\", \"Tierney\", \"Tukey\", \"Venables\"), class = \"factor\"), nationality = structure(integer(0), .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), deceased = structure(integer(0), .Label = c(\"no\", \"yes\"), class = \"factor\")), .Names = c(\"surname\", \"nationality\", \"deceased\"), row.names = integer(0), class = \"data.frame\"))"));        
do.call(`seq_along`, argv);        
}, o=expected);        

