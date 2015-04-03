expected <- eval(parse(text="NA_character_"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(NA_integer_, .Label = c(\"Australia\", \"UK\", \"US\"), class = \"factor\"), \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

