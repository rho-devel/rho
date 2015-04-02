expected <- eval(parse(text="structure(list(width = 80L, minIndent = 10L, extraIndent = 4L, sectionIndent = 5L, sectionExtra = 2L, itemBullet = \"• \", enumFormat = function (n) sprintf(\"%d. \", n), showURLs = FALSE, code_quote = TRUE, underline_titles = FALSE), .Names = c(\"width\", \"minIndent\", \"extraIndent\", \"sectionIndent\", \"sectionExtra\", \"itemBullet\", \"enumFormat\", \"showURLs\", \"code_quote\", \"underline_titles\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(width = 80L, minIndent = 10L, extraIndent = 4L, sectionIndent = 5L, sectionExtra = 2L, itemBullet = \"• \", enumFormat = function (n) sprintf(\"%d. \", n), showURLs = FALSE, code_quote = TRUE, underline_titles = FALSE), .Names = c(\"width\", \"minIndent\", \"extraIndent\", \"sectionIndent\", \"sectionExtra\", \"itemBullet\", \"enumFormat\", \"showURLs\", \"code_quote\", \"underline_titles\")))"));      
do.call(`invisible`, argv);      
}, o=expected);      

