expected <- eval(parse(text="structure(list(`NA` = NULL), .Names = NA_character_)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(Fertility = c(80.2, 83.1, 92.5, 85.8, 76.9), Agriculture = c(17, 45.1, 39.7, 36.5, 43.5), Examination = c(15L, 6L, 5L, 12L, 17L), Education = c(12L, 9L, 5L, 7L, 15L)), .Names = c(\"Fertility\", \"Agriculture\", \"Examination\", \"Education\"), row.names = c(\"Courtelary\", \"Delemont\", \"Franches-Mnt\", \"Moutier\", \"Neuveville\"), class = \"data.frame\"), \"Ferti\")"));                
do.call(`.subset`, argv);                
}, o=expected);                

