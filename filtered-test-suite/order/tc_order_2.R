expected <- eval(parse(text="1:14"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, structure(c(1, 2, 2.1, 2.3, 2.3, 3, 4, 5, 7, 8, 11, 13, 14, 15), .Names = c(\"\\\\title\", \"\\\\name\", \"\\\\alias\", \"\\\\keyword\", \"\\\\keyword\", \"\\\\description\", \"\\\\usage\", \"\\\\arguments\", \"\\\\details\", \"\\\\value\", \"\\\\author\", \"\\\\references\", \"\\\\seealso\", \"\\\\examples\")), c(\"\", \"\", \"LOGLIN\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\"), c(\"\", \"\", \"loglin\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\"))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));             
}, o=expected);             

