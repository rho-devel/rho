expected <- eval(parse(text="structure(c(\"data\", \"html\", \"po/en@quot/LC_MESSAGES\", \"po/en@quot\", \"po/pl/LC_MESSAGES\", \"po/pl\", \"po/de/LC_MESSAGES\", \"po/de\", \"po\", \"doc/SuiteSparse\", \"doc\", \"Meta\", \"include\", \"R\", \"help\", \"libs\", \"external\"), class = \"AsIs\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(\"./\", \"\", structure(c(\"./data\", \"./html\", \"./po/en@quot/LC_MESSAGES\", \"./po/en@quot\", \"./po/pl/LC_MESSAGES\", \"./po/pl\", \"./po/de/LC_MESSAGES\", \"./po/de\", \"./po\", \"./doc/SuiteSparse\", \"./doc\", \"./Meta\", \"./include\", \"./R\", \"./help\", \"./libs\", \"./external\"), class = \"AsIs\"), FALSE, FALSE, FALSE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

