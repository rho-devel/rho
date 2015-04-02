expected <- eval(parse(text="structure(\"Prediction matrix for soap film smooth\", Rd_tag = \"TEXT\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(\"Prediction matrix for soap film smooth\", Rd_tag = \"TEXT\"), \"UTF-8\", \"ASCII\", NA_character_, FALSE, FALSE)"));           
.Internal(iconv(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

